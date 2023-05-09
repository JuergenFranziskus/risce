use crate::assembler::object_file::Operation;
use std::{
    collections::{HashMap, HashSet},
    ops::{Add, Mul, Sub},
};
use super::object_file::{AbsoluteLabel, Calculation, ObjectFile, RelocFormat};


pub struct Linker<'a, 'b> {
    values: HashMap<CalcID, Value>,
    symbols: HashMap<AbsoluteLabel<'a>, SymbolID>,
    text_starts: HashMap<ObjID, u32>,
    data_starts: HashMap<ObjID, u32>,
    bss_starts: HashMap<ObjID, u32>,
    global_bss_start: u32,
    total_bss_size: Option<Value>,
    bytes: Vec<u8>,
    files: &'b [ObjectFile<'a>],
}
impl<'a, 'b> Linker<'a, 'b> {
    pub fn new(files: &'b [ObjectFile<'a>]) -> Self {
        Self {
            values: HashMap::new(),
            symbols: HashMap::new(),
            text_starts: HashMap::new(),
            data_starts: HashMap::new(),
            bss_starts: HashMap::new(),
            global_bss_start: 0xdeadbeef,
            total_bss_size: None,
            bytes: Vec::new(),
            files,
        }
    }

    pub fn link(mut self) -> Vec<u8> {
        self.copy_text_sections();
        self.copy_data_sections();
        self.set_global_bss_start();
        self.apply_relocations();

        self.bytes
    }
    fn copy_text_sections(&mut self) {
        for (i, file) in self.files.iter().enumerate() {
            self.text_starts.insert(ObjID(i), self.bytes.len() as u32);
            self.bytes.extend(&file.text);
        }
    }
    fn copy_data_sections(&mut self) {
        for (i, file) in self.files.iter().enumerate() {
            while self.bytes.len() % 4 != 0 {
                self.bytes.push(0);
            }

            self.data_starts.insert(ObjID(i), self.bytes.len() as u32);
            self.bytes.extend(&file.data);
        }
    }
    fn set_global_bss_start(&mut self) {
        let mut start = self.bytes.len() as u32;
        while start % 4 != 0 {
            start += 1;
        }
        self.global_bss_start = start;
    }
    fn apply_relocations(&mut self) {
        for (o, file) in self.files.iter().enumerate() {
            for (i, reloc) in file.relocations.iter().enumerate() {
                let reloc_id = RelocID(ObjID(o), i);
                let address = self
                    .calculate(CalcID::RelocAddress(reloc_id), None)
                    .to_u32();

                let value = self.calculate(CalcID::RelocValue(reloc_id), Some(address));

                let bytes = encode(value, reloc.format);
                for (i, byte) in bytes.into_iter().enumerate() {
                    let address = address as usize + i;
                    if address < self.bytes.len() {
                        self.bytes[address] |= byte;
                    }
                }
            }
        }
    }

    fn calculate(&mut self, id: CalcID, in_reloc: Option<u32>) -> Value {
        let mut in_calcs = HashSet::new();
        self.calculate_prime(id, in_reloc, &mut in_calcs)
    }
    fn calculate_prime(
        &mut self,
        id: CalcID,
        in_reloc: Option<u32>,
        in_calcs: &mut HashSet<CalcID>,
    ) -> Value {
        if in_calcs.contains(&id) {
            panic!();
        }

        if let Some(&value) = self.values.get(&id) {
            return value;
        }

        in_calcs.insert(id);
        let program = self.get_calculation(id);
        let mut value = self
            .eval_program(program, id.obj_id(), in_reloc, in_calcs)
            .to_u32();
        in_calcs.remove(&id);

        if let CalcID::BssSize(_) = id {
            while value % 4 != 0 {
                value += 1;
            }
        }

        self.values.insert(id, Value(value.to_le_bytes()));
        value.into()
    }

    fn eval_program(
        &mut self,
        program: &'b Calculation<'a>,
        obj: ObjID,
        in_reloc: Option<u32>,
        in_calcs: &mut HashSet<CalcID>,
    ) -> Value {
        let mut stack = Vec::new();

        for &op in &program.0 {
            use Operation::*;
            match op {
                Constant(val) => stack.push(Value(val.to_le_bytes())),
                SConstant(val) => stack.push(Value(val.to_le_bytes())),
                Symbol(name) => {
                    let id = self.get_symbol_id(name);
                    let calc = CalcID::SymbolValue(id);
                    stack.push(self.calculate_prime(calc, None, in_calcs));
                }
                LocalTextSectionStart => stack.push(self.text_starts[&obj].into()),
                LocalDataSectionStart => stack.push(self.data_starts[&obj].into()),
                LocalBssSectionStart => stack.push(self.calculate_bss_start(obj, in_calcs)),
                BssStart => stack.push(Value(self.global_bss_start.to_le_bytes())),
                BssEnd => {
                    let start = self.global_bss_start;
                    let size = self.calculate_total_bss_size(in_calcs).to_u32();
                    let end = start + size;
                    stack.push(end.into());
                }

                MakeRelative => {
                    let address = in_reloc.unwrap().into();
                    let value = stack.pop().unwrap();
                    stack.push(value - address);
                }

                Add => {
                    let b = stack.pop().unwrap();
                    let a = stack.pop().unwrap();
                    stack.push(a + b);
                }
                Sub => {
                    let b = stack.pop().unwrap();
                    let a = stack.pop().unwrap();
                    stack.push(a - b);
                }
                Mul => {
                    let b = stack.pop().unwrap();
                    let a = stack.pop().unwrap();
                    stack.push(a * b);
                }
                Div => {
                    let b = stack.pop().unwrap();
                    let a = stack.pop().unwrap();
                    stack.push(Value::div(a, b));
                }
                IDiv => {
                    let b = stack.pop().unwrap();
                    let a = stack.pop().unwrap();
                    stack.push(Value::idiv(a, b));
                }
                Rem => {
                    let b = stack.pop().unwrap();
                    let a = stack.pop().unwrap();
                    stack.push(Value::rem(a, b));
                }
                IRem => {
                    let b = stack.pop().unwrap();
                    let a = stack.pop().unwrap();
                    stack.push(Value::irem(a, b));
                }
                Shl => {
                    let b = stack.pop().unwrap();
                    let a = stack.pop().unwrap();
                    stack.push(Value::shl(a, b));
                }

                Low => {
                    let val = stack.pop().unwrap().to_i32();
                    let low = get_low_high(val).0;
                    stack.push(low.into());
                }
                High => {
                    let val = stack.pop().unwrap().to_i32();
                    let high = get_low_high(val).1;
                    stack.push(high.into());
                }
            }
        }

        stack.pop().unwrap()
    }
    fn calculate_bss_start(&mut self, obj: ObjID, in_calcs: &mut HashSet<CalcID>) -> Value {
        if let Some(&start) = self.bss_starts.get(&obj) {
            return Value(start.to_le_bytes());
        }

        let range = 0..obj.0;
        let mut sum = 0;
        for i in range {
            let calc = CalcID::BssSize(ObjID(i));
            let value = self.calculate_prime(calc, None, in_calcs);
            sum += value.to_u32();
        }

        self.bss_starts.insert(obj, self.global_bss_start + sum);
        sum.into()
    }
    fn calculate_total_bss_size(&mut self, in_calcs: &mut HashSet<CalcID>) -> Value {
        if let Some(res) = self.total_bss_size {
            return res;
        }

        let range = 0..self.files.len();
        let mut sum = Value::zero();
        for i in range {
            let calc = CalcID::BssSize(ObjID(i));
            sum = sum + self.calculate_prime(calc, None, in_calcs);
        }

        self.total_bss_size = Some(sum);
        sum
    }

    fn get_symbol_id(&mut self, name: AbsoluteLabel<'a>) -> SymbolID {
        if let Some(&id) = self.symbols.get(&name) {
            return id;
        }

        for (o, file) in self.files.iter().enumerate() {
            for (i, symbol) in file.symbols.iter().enumerate() {
                if symbol.name == name {
                    let id = SymbolID(ObjID(o), i);
                    self.symbols.insert(name, id);
                    return id;
                }
            }
        }

        panic!("Could not find symbol {name:?}");
    }

    fn get_calculation(&self, id: CalcID) -> &'b Calculation<'a> {
        use CalcID::*;
        match id {
            BssSize(obj) => &self.files[obj.0].bss_size,
            SymbolValue(SymbolID(obj, i)) => &self.files[obj.0].symbols[i].value,
            RelocAddress(RelocID(obj, i)) => &self.files[obj.0].relocations[i].address,
            RelocValue(RelocID(obj, i)) => &self.files[obj.0].relocations[i].value,
        }
    }
}

fn encode(value: Value, format: RelocFormat) -> [u8; 4] {
    let u = value.to_u32();
    let s = value.to_i32();

    use RelocFormat::*;
    match format {
        Byte => {
            assert!(u < 256 || (-128..128).contains(&s));
            [value.0[0], 0, 0, 0]
        }
        Short => {
            assert!(u < 65536 || (-32768..32768).contains(&s));
            [value.0[0], value.0[1], 0, 0]
        }
        Word => value.0,
        IFormatB => {
            assert!((-2048..2048).contains(&s), "({s}:{u}) does not fit into 12 bits");
            (u << 20).to_le_bytes()
        }
        IFormatC => {
            assert!((-524288..524288).contains(&s), "({s}:{u}) does not fit into 20 bits");
            (u << 12).to_le_bytes()
        }
        IFormatD => {
            assert!((-2048..2048).contains(&s));
            let mask = 0b11111;
            let low = (u & mask) << 7;
            let high = (u & !mask) << (25 - 5);
            (low | high).to_le_bytes()
        }
    }
}

fn round_nearest_multiple(value: i32, multiple: i32) -> i32 {
    let half = multiple / 2;
    let offset = if value.is_negative() {
        value - half
    } else {
        value + half
    };
    offset / 2 * 2
}
fn get_low_high(value: i32) -> (i32, i32) {
    let high = round_nearest_multiple(value, 4096) / 4096;
    let low = value - (high << 12);
    assert_eq!(low.wrapping_add(high << 12), value, "Your math is shit");

    (low, high)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Value([u8; 4]);
impl Value {
    pub fn to_u32(self) -> u32 {
        u32::from_le_bytes(self.0)
    }
    pub fn to_i32(self) -> i32 {
        i32::from_le_bytes(self.0)
    }

    fn div(a: Self, b: Self) -> Self {
        let a = a.to_u32();
        let b = b.to_u32();

        let res = a.wrapping_div(b);
        Self(res.to_le_bytes())
    }
    fn rem(a: Self, b: Self) -> Self {
        let a = a.to_u32();
        let b = b.to_u32();

        let res = a.wrapping_rem(b);
        Self(res.to_le_bytes())
    }
    fn idiv(a: Self, b: Self) -> Self {
        let a = a.to_i32();
        let b = b.to_i32();

        let res = a.wrapping_div(b);
        Self(res.to_le_bytes())
    }
    fn irem(a: Self, b: Self) -> Self {
        let a = a.to_i32();
        let b = b.to_i32();

        let res = a.wrapping_rem(b);
        Self(res.to_le_bytes())
    }
    fn shl(a: Value, b: Value) -> Value {
        let a = a.to_u32();
        let b = b.to_u32();
        (a << b).into()
    }

    fn zero() -> Value {
        Value([0, 0, 0, 0])
    }

    
}
impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Self(value.to_le_bytes())
    }
}
impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Self(value.to_le_bytes())
    }
}
impl Add for Value {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        let a = self.to_u32();
        let b = rhs.to_u32();

        let res = a.wrapping_add(b);
        Self(res.to_le_bytes())
    }
}
impl Sub for Value {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        let a = self.to_u32();
        let b = rhs.to_u32();

        let res = a.wrapping_sub(b);
        Self(res.to_le_bytes())
    }
}
impl Mul for Value {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        let a = self.to_u32();
        let b = rhs.to_u32();

        let res = a.wrapping_mul(b);
        Self(res.to_le_bytes())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum CalcID {
    BssSize(ObjID),
    SymbolValue(SymbolID),
    RelocAddress(RelocID),
    RelocValue(RelocID),
}
impl CalcID {
    fn obj_id(self) -> ObjID {
        match self {
            Self::BssSize(o) => o,
            Self::SymbolValue(s) => s.0,
            Self::RelocAddress(r) => r.0,
            Self::RelocValue(r) => r.0,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct SymbolID(ObjID, usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct RelocID(ObjID, usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct ObjID(usize);
