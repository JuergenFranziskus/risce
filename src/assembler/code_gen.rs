use super::{
    ast::{
        Arg, ArgKind, Ast, BinaryExpr, Condition, DBArg, Expr, ExprKind, Function, Line, LineKind,
        MemSize, Mnemonic, Register,
    },
    object_file::{
        AbsoluteLabel, Calculation, ObjectFile, RelocFormat, Relocation, Section, Symbol,
    },
    token::Identifier,
};
use crate::assembler::ast::DBArgKind;
use std::mem::take;

pub struct CodeGen<'a> {
    text: Vec<u8>,
    data: Vec<u8>,
    bss_size: Calculation<'a>,
    section: Section,
    last_global: Option<&'a str>,
    symbols: Vec<Symbol<'a>>,
    relocations: Vec<Relocation<'a>>,
}
impl<'a> CodeGen<'a> {
    pub fn new() -> Self {
        CodeGen {
            text: Vec::new(),
            data: Vec::new(),
            bss_size: Calculation::start(),
            section: Section::Text,
            last_global: None,
            symbols: Vec::new(),
            relocations: Vec::new(),
        }
    }

    pub fn gen_code(mut self, ast: &Ast<'a>) -> ObjectFile<'a> {
        self.bss_size.constant(0);

        for line in &ast.lines {
            self.gen_line(line);
        }

        ObjectFile {
            symbols: self.symbols,
            relocations: self.relocations,
            text: self.text,
            data: self.data,
            bss_size: self.bss_size,
        }
    }

    fn gen_line(&mut self, line: &Line<'a>) {
        if let Some(label) = line.label {
            self.process_line_label(label);
        }

        match &line.kind {
            LineKind::Empty => (),
            &LineKind::Equ(name, ref value) => self.gen_equ(name, value),
            &LineKind::Op(mnemonic, ref args) => self.gen_op(mnemonic, args),
            LineKind::DB(args) => self.gen_db(args),
            LineKind::ResW(bytes) => self.gen_resw(bytes),
            &LineKind::Section(name) => self.process_section(name),
        }
    }
    fn process_line_label(&mut self, label: Identifier<'a>) {
        let export = label.is_global();
        let label = self.make_label_absolute(label);
        let value = self.curr_address();

        if export {
            self.last_global = Some(label.global);
        }

        self.symbols.push(Symbol {
            name: label,
            value,
            exported: export,
        })
    }
    fn gen_op(&mut self, op: Mnemonic, args: &[Arg<'a>]) {
        match op {
            Mnemonic::Mov => self.gen_mov(args),
            Mnemonic::Lui => self.gen_lui(args),
            Mnemonic::Jmp => self.gen_jmp(args),
            Mnemonic::Jal => self.gen_jal(args),
            Mnemonic::Call => self.gen_call(args),
            Mnemonic::Ret => self.gen_ret(args),
            Mnemonic::Branch(op) => self.gen_branch(op, args),
            Mnemonic::Store => self.gen_store(args),
            Mnemonic::Load => self.gen_load(args),
            Mnemonic::Lea => self.gen_lea(args),
            Mnemonic::Not => self.gen_not(args),
            Mnemonic::Neg => self.gen_neg(args),
            Mnemonic::Add => self.gen_bin_alu_op(args, ALU_ADD),
            Mnemonic::Sub => self.gen_bin_alu_op(args, ALU_SUB),
            Mnemonic::And => self.gen_bin_alu_op(args, ALU_AND),
            Mnemonic::Or => self.gen_bin_alu_op(args, ALU_OR),
            Mnemonic::Nand => self.gen_bin_alu_op(args, ALU_NAND),
            Mnemonic::Xor => self.gen_bin_alu_op(args, ALU_XOR),
            Mnemonic::Shl => self.gen_bin_alu_op(args, ALU_SHL),
            Mnemonic::Shr => self.gen_bin_alu_op(args, ALU_SHR),
            Mnemonic::Sar => self.gen_bin_alu_op(args, ALU_SAR),
            Mnemonic::Rol => self.gen_bin_alu_op(args, ALU_ROL),
            Mnemonic::Ror => self.gen_bin_alu_op(args, ALU_ROR),
            Mnemonic::Mul => self.gen_bin_alu_op(args, ALU_MUL),
            Mnemonic::UMul => self.gen_bin_alu_op(args, ALU_UMUL),
            Mnemonic::IMul => self.gen_bin_alu_op(args, ALU_IMUL),
            Mnemonic::UDiv => self.gen_bin_alu_op(args, ALU_UDIV),
            Mnemonic::IDiv => self.gen_bin_alu_op(args, ALU_IDIV),
            Mnemonic::URem => self.gen_bin_alu_op(args, ALU_UREM),
            Mnemonic::IRem => self.gen_bin_alu_op(args, ALU_IREM),
            Mnemonic::Set(con) => self.gen_cmp(con, args),
            Mnemonic::Enter => self.gen_enter(args),
            Mnemonic::Leave => self.gen_leave(args),
        }
    }
    fn gen_equ(&mut self, name: Identifier<'a>, value: &Expr<'a>) {
        let label = self.make_label_absolute(name);
        let mut program = Calculation::start();
        self.relocate_prime(value, &mut program);

        self.symbols.push(Symbol {
            name: label,
            value: program,
            exported: false,
        })
    }
    fn gen_db(&mut self, args: &[DBArg<'a>]) {
        assert_eq!(self.section, Section::Data);

        for arg in args {
            match &arg.kind {
                &DBArgKind::StringLiteral(lit) => self.data.extend(lit.as_bytes()),
                DBArgKind::Expression(e) => {
                    self.relocate(e, RelocFormat::Byte, false);
                    self.data.push(0);
                }
            }
        }
    }
    fn gen_resw(&mut self, bytes: &Expr<'a>) {
        assert_eq!(self.section, Section::Bss);

        let mut program = take(&mut self.bss_size);
        self.relocate_prime(bytes, &mut program);
        program.constant(4);
        program.mul();
        program.add();
        self.bss_size = program;
    }
    fn process_section(&mut self, name: &str) {
        match name {
            "text" => self.section = Section::Text,
            "data" => self.section = Section::Data,
            "bss" => self.section = Section::Bss,
            _ => panic!(),
        }
    }

    fn push_instruction(&mut self, bytes: [u8; 4]) {
        assert_eq!(self.section, Section::Text);
        self.text.extend(bytes);
    }

    fn gen_not(&mut self, args: &[Arg<'a>]) {
        assert!(args.len() <= 2);
        let ArgKind::Register(d) = args[0].kind else { panic!() };
        if args.len() == 2 {
            let ArgKind::Register(a) = args[1].kind else { panic!() };
            self.gen_reg_reg_alu_op(d, a, a, ALU_NAND, 0);
        } else {
            self.gen_reg_reg_alu_op(d, d, d, ALU_NAND, 0);
        }
    }
    fn gen_neg(&mut self, args: &[Arg<'a>]) {
        assert!(args.len() <= 2);
        let ArgKind::Register(d) = args[0].kind else { panic!() };
        if args.len() == 2 {
            let ArgKind::Register(a) = args[1].kind else { panic!() };
            self.gen_reg_reg_alu_op(d, a, Register(0), ALU_NEG, 0);
        } else {
            self.gen_reg_reg_alu_op(d, d, Register(0), ALU_NEG, 0);
        }
    }
    fn gen_bin_alu_op(&mut self, args: &[Arg<'a>], op: u8) {
        if args.len() == 3 {
            let ArgKind::Register(d) = args[0].kind else { panic!() };

            use ArgKind::*;
            match (&args[1].kind, &args[2].kind) {
                (&Register(a), &Register(b)) => self.gen_reg_reg_alu_op(d, a, b, op, 0),
                (&Register(a), Expression(e)) => self.gen_reg_imm_alu_op(d, a, e, op),
                (Expression(e), &Register(a)) => self.gen_imm_reg_alu_op(d, a, e, op),
                _ => panic!(),
            }
        } else if args.len() == 2 {
            let ArgKind::Register(d) = args[0].kind else { panic!("Expected register, found {:?}", args[0].kind) };
            if let ArgKind::Register(a) = args[1].kind {
                self.gen_reg_reg_alu_op(d, d, a, op, 0);
            } else if let ArgKind::Expression(e) = &args[1].kind {
                self.gen_reg_imm_alu_op(d, d, e, op);
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }
    fn gen_mov(&mut self, args: &[Arg<'a>]) {
        assert!(args.len() == 2);
        let ArgKind::Register(d) = args[0].kind else { panic!() };
        if let ArgKind::Register(a) = args[1].kind {
            self.gen_reg_reg_alu_op(d, a, Register(0), ALU_ADD, 0);
        } else if let ArgKind::Expression(e) = &args[1].kind {
            self.gen_load_immediate(d, e);
        } else {
            panic!()
        }
    }
    fn gen_cmp(&mut self, op: Condition, args: &[Arg<'a>]) {
        let condition = op as u8;

        let ArgKind::Register(d) = args[0].kind else { panic!() };
        let ArgKind::Register(a) = args[1].kind else { panic!() };

        if args.len() == 3 {
            let ArgKind::Register(b) = args[2].kind else { panic!() };
            self.gen_reg_reg_alu_op(d, a, b, ALU_SET, condition);
        } else if args.len() == 2 {
            self.gen_reg_reg_alu_op(d, d, a, ALU_SET, condition);
        } else {
            panic!()
        }
    }

    fn gen_reg_reg_alu_op(
        &mut self,
        d: Register,
        a: Register,
        b: Register,
        alu_op: u8,
        condition: u8,
    ) {
        debug_assert!(alu_op < 32);
        debug_assert!(condition < 16);
        let regs = encode_registers(Some(d), Some(a), Some(b));
        let op = (alu_op as u32) << 22;
        let condition = (condition as u32) << 27;

        let instruction = regs | op | condition;
        let bytes = instruction.to_le_bytes();
        self.push_instruction(bytes);
    }
    fn gen_reg_imm_alu_op(&mut self, d: Register, a: Register, imm: &Expr<'a>, op: u8) {
        self.relocate(imm, RelocFormat::IFormatB, false);
        self.gen_reg_imm_alu_op_prime(d, a, 0, op);
    }
    fn gen_imm_reg_alu_op(&mut self, d: Register, a: Register, imm: &Expr<'a>, op: u8) {
        debug_assert!(op < 32);
        let op_bits = (op as u32 & 0b111) << 17;
        let opcode = match op {
            0..=7 => 0x1C,
            8..=15 => 0x1D,
            16..=23 => 0x1E,
            24..=31 => 0x1F,
            _ => unreachable!(),
        };
        self.relocate(imm, RelocFormat::IFormatB, false);
        let regs = encode_registers(Some(d), Some(a), None);

        let instruction = regs | opcode | op_bits;
        let bytes = instruction.to_le_bytes();
        self.push_instruction(bytes);
    }
    fn gen_reg_imm_alu_op_prime(&mut self, d: Register, a: Register, imm: i32, op: u8) {
        debug_assert!(op < 32);
        let op_bits = (op as u32 & 0b111) << 17;
        let opcode = match op {
            0..=7 => 16,
            8..=15 => 17,
            16..=23 => 18,
            24..=31 => 19,
            _ => unreachable!(),
        };
        let regs = encode_registers(Some(d), Some(a), None);
        self.relocate_constant(imm, RelocFormat::IFormatB);

        let instruction = regs | opcode | op_bits;
        let bytes = instruction.to_le_bytes();
        self.push_instruction(bytes);
    }

    fn gen_load_immediate(&mut self, d: Register, val: &Expr<'a>) {
        let opcode = 0x20;
        let regs = encode_d_register(d);
        self.relocate(val, RelocFormat::IFormatC, false);
        let instruction = opcode | regs;
        let bytes = instruction.to_le_bytes();
        self.push_instruction(bytes);
    }
    fn gen_lui(&mut self, args: &[Arg<'a>]) {
        assert!(args.len() == 2);
        let ArgKind::Register(d) = args[0].kind else { panic!() };
        let ArgKind::Expression(e) = &args[1].kind else { panic!() };

        let opcode = 0x21;
        let regs = encode_d_register(d);
        self.relocate(e, RelocFormat::IFormatC, false);

        let instruction = opcode | regs;
        let bytes = instruction.to_le_bytes();
        self.push_instruction(bytes);
    }

    fn gen_call(&mut self, args: &[Arg<'a>]) {
        let d = Register(31);
        if args.len() == 2 {
            let ArgKind::Register(a) = args[0].kind else { panic!() };
            let ArgKind::Expression(e) = &args[1].kind else { panic!() };
            self.gen_jump_absolute_offset(d, a, Some(e));
        } else if args.len() == 1 {
            if let ArgKind::Register(a) = args[0].kind {
                self.gen_jump_absolute_offset(d, a, None);
            } else if let ArgKind::Expression(e) = &args[0].kind {
                self.gen_jump_relative(d, e);
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }
    fn gen_ret(&mut self, args: &[Arg<'a>]) {
        assert_eq!(args.len(), 0);
        self.gen_jump_absolute_offset(Register(0), Register(31), None)
    }
    fn gen_jmp(&mut self, args: &[Arg<'a>]) {
        if args.len() == 2 {
            let ArgKind::Register(a) = args[0].kind else { panic!() };
            let ArgKind::Expression(e) = &args[1].kind else { panic!() };
            self.gen_jump_absolute_offset(Register(0), a, Some(e));
        } else if args.len() == 1 {
            if let ArgKind::Register(a) = args[0].kind {
                self.gen_jump_absolute_offset(Register(0), a, None);
            } else if let ArgKind::Expression(e) = &args[0].kind {
                self.gen_jump_relative(Register(0), e);
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }
    fn gen_jal(&mut self, args: &[Arg<'a>]) {
        let ArgKind::Register(d) = args[0].kind else { panic!() };
        if args.len() == 3 {
            let ArgKind::Register(a) = args[1].kind else { panic!() };
            let ArgKind::Expression(e) = &args[2].kind else { panic!() };
            self.gen_jump_absolute_offset(d, a, Some(e));
        } else if args.len() == 2 {
            if let ArgKind::Register(a) = args[1].kind {
                self.gen_jump_absolute_offset(d, a, None);
            } else if let ArgKind::Expression(e) = &args[1].kind {
                self.gen_jump_relative(d, e);
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }
    fn gen_jump_absolute_offset(&mut self, d: Register, a: Register, e: Option<&Expr<'a>>) {
        if let Some(e) = e {
            self.relocate(e, RelocFormat::IFormatB, false);
        }
        let regs = encode_registers(Some(d), Some(a), None);
        let opcode = 0x1B;

        let instruction = opcode | regs;
        let bytes = instruction.to_le_bytes();
        self.push_instruction(bytes);
    }
    fn gen_jump_relative(&mut self, d: Register, e: &Expr<'a>) {
        self.relocate(e, RelocFormat::IFormatC, true);

        let regs = encode_registers(Some(d), None, None);
        let opcode = 0x22;

        let instruction = opcode | regs;
        let bytes = instruction.to_le_bytes();
        self.push_instruction(bytes);
    }

    fn gen_branch(&mut self, condition: Condition, args: &[Arg<'a>]) {
        use Condition::*;
        assert_eq!(args.len(), 3);
        let ArgKind::Register(a) = args[0].kind else { panic!() };
        let ArgKind::Register(b) = args[1].kind else { panic!() };
        let ArgKind::Expression(e) = &args[2].kind else { panic!() };
        self.relocate(e, RelocFormat::IFormatD, true);

        let (opcode, branchop) = match condition {
            Greater => (0x36, 0),
            Less => (0x36, 1),
            Above => (0x36, 2),
            Below => (0x36, 3),
            NotGreater => (0x36, 4),
            NotLess => (0x36, 5),
            NotAbove => (0x36, 6),
            NotBelow => (0x36, 7),
            Equal => (0x37, 0),
            NotEqual => (0x37, 0),
        };
        let branchop = branchop << 22;
        let regs = encode_registers(None, Some(a), Some(b));

        let instruction = opcode | branchop | regs;
        let bytes = instruction.to_le_bytes();
        self.push_instruction(bytes);
    }

    fn gen_lea(&mut self, args: &[Arg<'a>]) {
        assert!(args.len() == 2);
        let ArgKind::Register(d) = args[0].kind else { panic!() };
        let ArgKind::Memory(mem) = &args[1].kind else { panic!() };

        let a = mem.base.unwrap_or(Register(0));
        let offset = mem.offset.as_ref();
        let is_relative = mem.rip_relative;
        assert!(is_relative);
        let regs = encode_registers(Some(d), Some(a), None);
        if let Some(val) = offset {
            self.relocate(val, RelocFormat::IFormatB, false);
        }
        let opcode = 0x17;

        let instruction = opcode | regs;
        let bytes = instruction.to_le_bytes();
        self.push_instruction(bytes);
    }
    fn gen_store(&mut self, args: &[Arg<'a>]) {
        assert!(args.len() == 2);
        let ArgKind::Register(b) = args[0].kind else { panic!() };
        let ArgKind::Memory(mem) = &args[1].kind else { panic!() };

        let a = mem.base.unwrap_or(Register(0));
        let offset = mem.offset.as_ref();
        let is_relative = mem.rip_relative;
        let size = mem.size.unwrap_or(MemSize::Word);

        if let Some(val) = offset {
            self.relocate(val, RelocFormat::IFormatD, false);
        }

        self.gen_store_prime(a, b, 0, is_relative, size);
    }
    fn gen_store_prime(
        &mut self,
        a: Register,
        b: Register,
        offset: i32,
        is_relative: bool,
        size: MemSize,
    ) {
        let opcode = if is_relative { 0x30 } else { 0x33 };
        let regs = encode_registers(None, Some(a), Some(b));
        self.relocate_constant(offset, RelocFormat::IFormatD);
        let size = size.to_store_bits();

        let instruction = opcode | regs | size;
        let bytes = instruction.to_le_bytes();
        self.push_instruction(bytes);
    }
    fn gen_load(&mut self, args: &[Arg<'a>]) {
        assert!(args.len() == 2);
        let ArgKind::Register(d) = args[0].kind else { panic!() };
        let ArgKind::Memory(mem) = &args[1].kind else { panic!() };

        let a = mem.base.unwrap_or(Register(0));
        let offset = mem.offset.as_ref();
        let is_relative = mem.rip_relative;
        let size = mem.size.unwrap_or(MemSize::Word);

        if let Some(val) = offset {
            self.relocate(val, RelocFormat::IFormatB, false);
        }

        self.gen_load_prime(d, a, 0, is_relative, size);
    }
    fn gen_load_prime(
        &mut self,
        d: Register,
        a: Register,
        offset: i32,
        is_relative: bool,
        size: MemSize,
    ) {
        let opcode = if is_relative { 0x14 } else { 0x18 };
        let regs = encode_registers(Some(d), Some(a), None);
        self.relocate_constant(offset, RelocFormat::IFormatB);
        let size = size.to_load_bits();

        let instruction = opcode | regs | size;
        let bytes = instruction.to_le_bytes();
        self.push_instruction(bytes);
    }

    fn gen_enter(&mut self, args: &[Arg<'a>]) {
        let mut registers = Vec::with_capacity(32);

        for arg in args {
            match &arg.kind {
                &ArgKind::Register(reg) => registers.push(reg),
                ArgKind::Expression(_e) => panic!(),
                ArgKind::Memory(_) => panic!(),
            }
        }
        let registers = if registers.is_empty() {
            DEFAULT_PUSH_REGS
        } else {
            &registers
        };

        let bytes = registers.len() as i32 * 4;
        let bytes = bytes + 4; // Also always save rbp

        self.gen_reg_imm_alu_op_prime(Register::rsp(), Register::rsp(), bytes, ALU_SUB); // Allocate stack space
        self.gen_store_prime(
            Register::rsp(),
            Register::rbp(),
            bytes - 4,
            false,
            MemSize::Word,
        ); // Save the old base pointer
        self.gen_reg_imm_alu_op_prime(Register::rbp(), Register::rsp(), bytes - 4, ALU_ADD); // Copy rsp to rbp

        for (i, &reg) in registers.iter().enumerate() {
            let i = i as i32;
            let offset = -i * 4 - 4;
            self.gen_store_prime(Register::rbp(), reg, offset, false, MemSize::Word);
        }
    }
    fn gen_leave(&mut self, args: &[Arg<'a>]) {
        let mut registers = Vec::with_capacity(32);
        for arg in args {
            match &arg.kind {
                &ArgKind::Register(reg) => registers.push(reg),
                ArgKind::Expression(_) => panic!(),
                ArgKind::Memory(_) => panic!(),
            }
        }
        let registers = if registers.is_empty() {
            DEFAULT_PUSH_REGS
        } else {
            &registers
        };

        for (i, &reg) in registers.iter().enumerate() {
            let i = i as i32;
            let offset = -i * 4 - 4;
            self.gen_load_prime(reg, Register::rbp(), offset, false, MemSize::Word);
        }

        self.gen_reg_reg_alu_op(Register::rsp(), Register::rbp(), Register(0), ALU_ADD, 0); // Copy rbp to rsp
        self.gen_load_prime(Register::rbp(), Register::rsp(), 0, false, MemSize::Word);
        self.gen_reg_imm_alu_op_prime(Register::rsp(), Register::rsp(), 4, ALU_ADD);
    }

    fn curr_address(&self) -> Calculation<'a> {
        let mut program = Calculation::start();
        match self.section {
            Section::Text => program.text_start(),
            Section::Data => program.data_start(),
            Section::Bss => program.bss_start(),
        };

        match self.section {
            Section::Text => program.constant(self.text.len() as u32),
            Section::Data => program.constant(self.data.len() as u32),
            Section::Bss => program.0.extend(self.bss_size.0.iter().copied()),
        }

        program.add();
        program
    }
    fn relocate_constant(&mut self, value: i32, format: RelocFormat) {
        if value == 0 {
            return;
        }

        let address = self.curr_address();
        let mut program = Calculation::start();
        program.sconstant(value);
        self.relocations.push(Relocation {
            address,
            value: program,
            format,
        })
    }
    fn relocate(&mut self, e: &Expr<'a>, format: RelocFormat, force_relative: bool) {
        let address = self.curr_address();
        let mut value = Calculation::start();

        self.relocate_prime(e, &mut value);

        if force_relative {
            value.make_relative();
        }

        let reloc = Relocation {
            address,
            value,
            format,
        };

        self.relocations.push(reloc);
    }
    fn relocate_prime(&self, e: &Expr<'a>, program: &mut Calculation<'a>) {
        match &e.kind {
            &ExprKind::Decimal(src) if src.contains('-') => program.sconstant(src.parse().unwrap()),
            &ExprKind::Decimal(src) => program.constant(src.parse().unwrap()),
            &ExprKind::Hex(src) if src.contains('-') => {
                program.sconstant(src[3..].parse::<i32>().unwrap().wrapping_neg())
            }
            &ExprKind::Hex(src) => program.constant(src[2..].parse().unwrap()),
            &ExprKind::Identifier(name) => match (name.name, name.local) {
                ("__bss_start", false) => program.global_bss_start(),
                ("__bss_end", false) => program.global_bss_end(),
                _ => program.symbol(self.make_label_absolute(name)),
            },
            &ExprKind::Call(function, ref args) => {
                args.iter().for_each(|a| self.relocate_prime(a, program));
                match function {
                    Function::Low => program.low(),
                    Function::High => program.high(),
                }
            }
            ExprKind::Paren(e) => self.relocate_prime(e, program),
            &ExprKind::Binary(op, ref a, ref b) => {
                self.relocate_prime(a, program);
                self.relocate_prime(b, program);
                match op {
                    BinaryExpr::Add => program.add(),
                    BinaryExpr::Sub => program.sub(),
                    BinaryExpr::Mul => program.mul(),
                }
            }
        }
    }

    fn make_label_absolute(&self, label: Identifier<'a>) -> AbsoluteLabel<'a> {
        if label.local {
            AbsoluteLabel {
                global: self.last_global.unwrap(),
                local: Some(label.name),
            }
        } else {
            label.name.into()
        }
    }
}
impl<'a> Default for CodeGen<'a> {
    fn default() -> Self {
        Self::new()
    }
}

const ALU_ADD: u8 = 2;
const ALU_SUB: u8 = 3;
const ALU_NEG: u8 = 4;
const ALU_AND: u8 = 5;
const ALU_OR: u8 = 6;
const ALU_NAND: u8 = 7;
const ALU_XOR: u8 = 8;
const ALU_SHL: u8 = 9;
const ALU_SHR: u8 = 10;
const ALU_SAR: u8 = 11;
const ALU_ROL: u8 = 12;
const ALU_ROR: u8 = 13;
const ALU_MUL: u8 = 0xE;
const ALU_UMUL: u8 = 0xF;
const ALU_IMUL: u8 = 0x10;
const ALU_UDIV: u8 = 0x11;
const ALU_UREM: u8 = 0x12;
const ALU_IDIV: u8 = 0x13;
const ALU_IREM: u8 = 0x14;
const ALU_SET: u8 = 0x15;

static DEFAULT_PUSH_REGS: &[Register] = &[
    Register(16),
    Register(17),
    Register(18),
    Register(19),
    Register(20),
    Register(21),
    Register(22),
    Register(23),
    Register(24),
    Register(25),
    Register(26),
    Register(27),
    Register(28),
    Register(31),
];

fn encode_d_register(d: Register) -> u32 {
    assert!(d.0 < 32);
    (d.0 as u32) << 7
}
fn encode_a_register(a: Register) -> u32 {
    assert!(a.0 < 32);
    (a.0 as u32) << 12
}
fn encode_b_register(b: Register) -> u32 {
    assert!(b.0 < 32);
    (b.0 as u32) << 17
}
fn encode_registers(d: Option<Register>, a: Option<Register>, b: Option<Register>) -> u32 {
    let d = d.unwrap_or(Register(0));
    let a = a.unwrap_or(Register(0));
    let b = b.unwrap_or(Register(0));

    encode_d_register(d) | encode_a_register(a) | encode_b_register(b)
}
