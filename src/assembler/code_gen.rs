use std::{collections::HashMap, str::FromStr};
use num::{BigInt, Num, ToPrimitive, Signed};
use super::{token::Identifier, ast::{Expr, Ast, Line, LineKind, Mnemonic, Arg, Register, ArgKind, MemSize, ExprKind, Function, Condition, BinaryExpr, UnaryExpr, DBArg, DBArgKind}};



pub struct CodeGen<'a> {
    labels: HashMap<Identifier<'a>, BigInt>,
    bytes: Vec<u8>,
    last_global: Option<&'a str>,
    relocations: Vec<Relocation<'a>>,
}
impl<'a> CodeGen<'a> {
    pub fn new() -> Self {
        CodeGen {
            labels: HashMap::new(),
            bytes: Vec::new(),
            last_global: None,
            relocations: Vec::new(),
        }
    }

    pub fn gen_code(mut self, ast: &Ast<'a>) -> Vec<u8> {
        for line in &ast.lines {
            self.gen_line(line);
        }
        self.apply_relocations();

        self.bytes
    }
    fn apply_relocations(&mut self) {
        for reloc in &self.relocations {
            let value = self.eval_expr(&reloc.value, reloc.in_context).unwrap();
            let value = reloc.kind.encode_immediate(value.to_i32().unwrap());
            let bytes = value.to_le_bytes();
            for (i, byte) in bytes.into_iter().enumerate() {
                let address = reloc.in_context.address as usize + i;
                self.bytes[address] |= byte;
            }
        }
    }

    fn gen_line(&mut self, line: &Line<'a>) {
        self.process_line_label(line.label);

        match &line.kind {
            LineKind::Empty => (),
            LineKind::Equ(e) => self.gen_equ(line.label, e),
            &LineKind::Op(mnemonic, ref args) => self.gen_op(mnemonic, args),
            &LineKind::DB(ref args) => self.gen_db(args),
        }
    }
    fn process_line_label(&mut self, label: Option<Identifier<'a>>) {
        if let Some(ident) = label {
            let ident = self.make_global(ident);
            self.last_global = ident.global;
            let address = self.bytes.len().into();
            self.labels.insert(ident, address);
        }
    }
    fn gen_equ(&mut self, label: Option<Identifier<'a>>, value: &Expr<'a>) {
        if let Some(ident) = label {
            let ident = self.make_global(ident);
            let ctx = self.curr_ctx();
            let value = self.eval_expr(value, ctx).unwrap();
            self.labels.insert(ident, value);
        }
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
        }
    }

    fn gen_db(&mut self, args: &[DBArg<'a>]) {
        for arg in args {
            match &arg.kind {
                &DBArgKind::StringLiteral(str) => self.bytes.extend(str.bytes()),
                &DBArgKind::Expression(ref expr) => {
                    let ctx = self.curr_ctx();
                    let val = self.eval_expr(expr, ctx).unwrap();
                    let val = val.to_u8().unwrap();
                    self.bytes.push(val);
                }
            }
        }
    }

    fn gen_not(&mut self, args: &[Arg<'a>]) {
        assert!(args.len() <= 2);
        let ArgKind::Register(d) = args[0].kind else { panic!() };
        if args.len() == 2 {
            let ArgKind::Register(a) = args[1].kind else { panic!() };
            self.gen_reg_reg_alu_op(d, a, a, ALU_NAND);
        }
        else {
            self.gen_reg_reg_alu_op(d, d, d, ALU_NAND);
        }
    }
    fn gen_neg(&mut self, args: &[Arg<'a>]) {
        assert!(args.len() <= 2);
        let ArgKind::Register(d) = args[0].kind else { panic!() };
        if args.len() == 2 {
            let ArgKind::Register(a) = args[1].kind else { panic!() };
            self.gen_reg_reg_alu_op(d, a, Register(0), ALU_NAND);
        }
        else {
            self.gen_reg_reg_alu_op(d, d, Register(0), ALU_NEG);
        }
    }
    fn gen_bin_alu_op(&mut self, args: &[Arg<'a>], op: u8) {
        if args.len() == 3 {
            let ArgKind::Register(d) = args[0].kind else { panic!() };
            let ArgKind::Register(a) = args[1].kind else { panic!() };
            
            if let ArgKind::Register(b) = args[2].kind {
                self.gen_reg_reg_alu_op(d, a, b, op);
            }
            else if let ArgKind::Expression(e) = &args[2].kind {
                self.gen_reg_imm_alu_op(d, a , e, op);
            }
            else {
                panic!()
            }
        }
        else if args.len() == 2 {
            let ArgKind::Register(d) = args[0].kind else { panic!("Expected register, found {:?}", args[0].kind) };
            if let ArgKind::Register(a) = args[1].kind {
                self.gen_reg_reg_alu_op(d, d, a, op);
            }
            else if let ArgKind::Expression(e) = &args[1].kind {
                self.gen_reg_imm_alu_op(d, d, e, op);
            }
            else {
                panic!()
            }
        }
        else {
            panic!()
        }
    }
    fn gen_mov(&mut self, args: &[Arg<'a>]) {
        assert!(args.len() == 2);
        let ArgKind::Register(d) = args[0].kind else { panic!() };
        if let ArgKind::Register(a) = args[1].kind {
            self.gen_reg_reg_alu_op(d, a, Register(0), ALU_ADD);
        }
        else if let ArgKind::Expression(e) = &args[1].kind {
            self.gen_load_immediate(d, e);
        }
        else {
            panic!()
        }
    }

    fn gen_reg_reg_alu_op(&mut self, d: Register, a: Register, b: Register, op: u8) {
        debug_assert!(op < 32);
        let regs = encode_registers(Some(d), Some(a), Some(b));
        let op = (op as u32) << 22;

        let instruction = regs | op;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }
    fn gen_reg_imm_alu_op(&mut self, d: Register, a: Register, imm: &Expr<'a>, op: u8) {
        debug_assert!(op < 32);
        let op_bits = (op as u32 & 0b111) << 17;
        let opcode = match op {
            0..=7 => 16,
            8..=15 => 17,
            16..=23 => 18,
            24..=31 => 19,
            _ => unreachable!()
        };
        let regs = encode_registers(Some(d), Some(a), None);
        self.reloc_b(imm);
        
        let instruction = regs | opcode | op_bits;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }

    fn gen_load_immediate(&mut self, d: Register, val: &Expr<'a>) {
        let opcode = 0x20;
        let regs = encode_d_register(d);
        self.reloc_c(val);

        let instruction = opcode | regs;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }
    fn gen_lui(&mut self, args: &[Arg<'a>]) {
        assert!(args.len() == 2);
        let ArgKind::Register(d) = args[0].kind else { panic!() };
        let ArgKind::Expression(e) = &args[1].kind else { panic!() };

        let opcode = 0x21;
        let regs = encode_d_register(d);
        self.reloc_c(e);

        let instruction = opcode | regs;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }

    fn gen_call(&mut self, args: &[Arg<'a>]) {
        let d = Register(31);
        if args.len() == 2 {
            let ArgKind::Register(a) = args[0].kind else { panic!() };
            let ArgKind::Expression(e) = &args[1].kind else { panic!() };
            self.gen_jump_absolute_offset(d, a, Some(e));
        }
        else if args.len() == 1 {
            if let ArgKind::Register(a) = args[0].kind {
                self.gen_jump_absolute_offset(d, a, None);
            }
            else if let ArgKind::Expression(e) = &args[0].kind {
                self.gen_jump_relative(d, e);
            }
            else {
                panic!()
            }
        }
        else {
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
        }
        else if args.len() == 1 {
            if let ArgKind::Register(a) = args[0].kind {
                self.gen_jump_absolute_offset(Register(0), a, None);
            }
            else if let ArgKind::Expression(e) = &args[0].kind {
                self.gen_jump_relative(Register(0), e);
            }
            else {
                panic!()
            }
        }
        else {
            panic!()
        }
    }
    fn gen_jal(&mut self, args: &[Arg<'a>]) {
        let ArgKind::Register(d) = args[0].kind else { panic!() };
        if args.len() == 3 {
            let ArgKind::Register(a) = args[1].kind else { panic!() };
            let ArgKind::Expression(e) = &args[2].kind else { panic!() };
            self.gen_jump_absolute_offset(d, a, Some(e));
        }
        else if args.len() == 2 {
            if let ArgKind::Register(a) = args[1].kind {
                self.gen_jump_absolute_offset(d, a, None);
            }
            else if let ArgKind::Expression(e) = &args[1].kind {
                self.gen_jump_relative(d, e);
            }
            else {
                panic!()
            }
        }
        else {
            panic!()
        }
    }
    fn gen_jump_absolute_offset(&mut self, d: Register, a: Register, e: Option<&Expr<'a>>) {
        let opcode = 0x1B;
        let regs = encode_registers(Some(d), Some(a), None);
        if let Some(e) = e {
            self.reloc_b(e);
        }

        let instruction = opcode | regs;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }
    fn gen_jump_relative(&mut self, d: Register, e: &Expr<'a>) {
        let opcode = 0x22;
        let regs = encode_d_register(d);
        self.reloc_c(e);

        let instruction = opcode | regs;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }

    fn gen_branch(&mut self, condition: Condition, args: &[Arg<'a>]) {
        use Condition::*;
        assert_eq!(args.len(), 3);
        let ArgKind::Register(a) = args[0].kind else { panic!() };
        let ArgKind::Register(b) = args[1].kind else { panic!() };
        let ArgKind::Expression(e) = &args[2].kind else { panic!() };
        self.reloc_d(e);

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
        self.bytes.extend(bytes);
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
            self.reloc_b(val);
        }
        let opcode = 0x17;

        let instruction = opcode | regs;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }
    fn gen_store(&mut self, args: &[Arg<'a>]) {
        assert!(args.len() == 2);
        let ArgKind::Register(b) = args[0].kind else { panic!() };
        let ArgKind::Memory(mem) = &args[1].kind else { panic!() };

        let a = mem.base.unwrap_or(Register(0));
        let offset = mem.offset.as_ref();
        let is_relative = mem.rip_relative;
        let size = mem.size.unwrap_or(MemSize::Word);

        let opcode = match (is_relative, size) {
            (true, MemSize::Byte) => 0x30,
            (true, MemSize::Short) => 0x31,
            (true, MemSize::Word) => 0x32,
            (false, MemSize::Byte) => 0x33,
            (false, MemSize::Short) => 0x34,
            (false, MemSize::Word) => 0x35,
        };
        let regs = encode_registers(None, Some(a), Some(b));
        if let Some(val) = offset {
            self.reloc_d(val);
        }
        
        let instruction = opcode | regs;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }
    fn gen_load(&mut self, args: &[Arg<'a>]) {
        assert!(args.len() == 2);
        let ArgKind::Register(d) = args[0].kind else { panic!() };
        let ArgKind::Memory(mem) = &args[1].kind else { panic!() };

        let a = mem.base.unwrap_or(Register(0));
        let offset = mem.offset.as_ref();
        let is_relative = mem.rip_relative;
        let size = mem.size.unwrap_or(MemSize::Word);

        let opcode = match (is_relative, size) {
            (true, MemSize::Byte) => 0x14,
            (true, MemSize::Short) => 0x15,
            (true, MemSize::Word) => 0x16,
            (false, MemSize::Byte) => 0x18,
            (false, MemSize::Short) => 0x19,
            (false, MemSize::Word) => 0x1A,
        };
        let regs = encode_registers(Some(d), Some(a), None);
        if let Some(val) = offset {
            self.reloc_b(val);
        }
        
        let instruction = opcode | regs;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }

    fn eval_expr(&self, e: &Expr, ctx: ExprCtx) -> Option<BigInt> {
        match &e.kind {
            &ExprKind::Here => Some(ctx.address.into()),
            &ExprKind::Identifier(i) => {
                let ident = make_global(ctx.last_global, i);
                self.labels.get(&ident).cloned()
            }
            &ExprKind::Decimal(name) => Some(BigInt::from_str(name).unwrap()),
            &ExprKind::Hex(name) => Some(BigInt::from_str_radix(&name[2..], 16).unwrap()),
            ExprKind::Paren(e) => self.eval_expr(e, ctx),
            &ExprKind::Call(func, ref args) => {
                let args = args.iter().map(|a| self.eval_expr(a, ctx)).collect::<Option<Vec<_>>>()?;
                Some(match func {
                    Function::Low =>  calculate_low(args , ctx),
                    Function::High => calculate_high(args, ctx),
                    Function::Rel =>  calculate_rel(args , ctx),
                    Function::Prep => calculate_prep(args, ctx),
                    Function::Fin =>  calculate_fin(args , ctx),
                })
            }
            &ExprKind::Binary(op, ref a, ref b) => self.eval_binary_expr(op, a, b, ctx),
            &ExprKind::Unary(op, ref a) => self.eval_unary_expr(op, a, ctx),
        }
    }
    fn eval_binary_expr(&self, op: BinaryExpr, a: &Expr, b: &Expr, ctx: ExprCtx) -> Option<BigInt> {
        let a = self.eval_expr(a, ctx)?;
        let b = self.eval_expr(b, ctx)?;

        Some(match op {
            BinaryExpr::Add => a + b,
            BinaryExpr::Sub => a - b,
            BinaryExpr::Mul => a * b,
            BinaryExpr::Div => a / b,
        })
    }
    fn eval_unary_expr(&self, op: UnaryExpr, a: &Expr, ctx: ExprCtx) -> Option<BigInt> {
        let a = self.eval_expr(a, ctx)?;
        match op {
            UnaryExpr::Relative => calculate_rel(vec!(a), ctx).into(),
            UnaryExpr::Finish => calculate_fin(vec!(a), ctx).into(),
        }
    }

    fn curr_ctx(&self) -> ExprCtx<'a> {
        ExprCtx {
            address: self.bytes.len() as u32,
            last_global: self.last_global,
        }
    }
    fn relocate(&mut self, kind: RelocKind, value: &Expr<'a>) {
        let ctx = self.curr_ctx();
        let reloc = Relocation {
            in_context: ctx,
            kind,
            value: value.clone(),
        };
        self.relocations.push(reloc);
    }
    fn reloc_b(&mut self, value: &Expr<'a>) {
        self.relocate(RelocKind::B, value);
    }
    fn reloc_c(&mut self, value: &Expr<'a>) {
        self.relocate(RelocKind::C, value);
    }
    fn reloc_d(&mut self, value: &Expr<'a>) {
        self.relocate(RelocKind::D, value);
    }

    fn make_global(&self, label: Identifier<'a>) -> Identifier<'a> {
        make_global(self.last_global, label)
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


fn make_global<'a>(globalizer: Option<&'a str>, mut label: Identifier<'a>) -> Identifier<'a> {
    let global = match (globalizer, label.global) {
        (_, Some(a)) => a,
        (Some(a), None) => a,
        (None, None) => panic!(),
    };
    label.global = Some(global);

    label
}


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

fn round_nearest_multiple(value: &BigInt, multiple: i32) -> BigInt {
    let half = multiple / 2;
    let offset = if value.is_negative() { value - half } else { value + half };
    offset / 2 * 2
}
fn get_low_high(value: BigInt) -> (BigInt, BigInt) {
    let high = round_nearest_multiple(&value, 4096) / 4096;
    let low = &value - (&high << 12);
    debug_assert_eq!(&low + (&high << 12), value, "Your math is shit");

    (low, high)
}
fn calculate_low(mut args: Vec<BigInt>, _ctx: ExprCtx) -> BigInt {
    assert_eq!(args.len(), 1);
    let value = args.pop().unwrap();

    get_low_high(value).0
}
fn calculate_high(mut args: Vec<BigInt>, _ctx: ExprCtx) -> BigInt {
    assert_eq!(args.len(), 1);
    let value = args.pop().unwrap();
    get_low_high(value).1
}
fn calculate_rel(mut args: Vec<BigInt>, ctx: ExprCtx) -> BigInt {
    assert!(args.len() <= 2);

    let base = if args.len() == 2 {
        args.pop().unwrap()
    }
    else {
        ctx.address.into()
    };
    let to_rebase = args.pop().unwrap();


    let relative = to_rebase - base;
    relative
}
fn calculate_prep(args: Vec<BigInt>, ctx: ExprCtx) -> BigInt {
    let rel = calculate_rel(args, ctx);
    get_low_high(rel).1
}
fn calculate_fin(args: Vec<BigInt>, ctx: ExprCtx) -> BigInt {
    let rel = calculate_rel(args, ctx);
    get_low_high(rel).0
}

struct Relocation<'a> {
    in_context: ExprCtx<'a>,
    kind: RelocKind,
    value: Expr<'a>,
}

enum RelocKind {
    B,
    C,
    D,
}
impl RelocKind {
    fn encode_immediate(&self, immediate: i32) -> u32 {
        let as_bits = u32::from_le_bytes(immediate.to_le_bytes());
        match self {
            Self::B => {
                assert!(immediate >= -2048 && immediate <= 2047, "{} does not fit into 12 bits", immediate);
                (as_bits & 0xFFFFFF) << 20
            }
            Self::C => {
                let min = -(2i32.pow(19));
                let max = 2i32.pow(19) - 1;
                assert!(immediate >= min && immediate <= max, "{immediate} does not fit into 20 bits");
                let value = as_bits & 0xFFFFF;
                value << 12
            }
            Self::D => {
                assert!(immediate >= -2048 && immediate <= 2047, "{immediate} does not fit into 12 bits");
                let value = as_bits & 0xFFFFFF;
                let low = (value & 0b11111) << 7;
                let high = (value & !0b11111) << (25 - 5);
                low | high
            }
        }
    }
}


#[derive(Copy, Clone, Debug)]
struct ExprCtx<'a> {
    address: u32,
    last_global: Option<&'a str>,
}
