use super::token::Identifier;
use serde_derive::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[derive(Serialize, Deserialize)]
pub struct ObjectFile<'a> {
    pub text: Vec<u8>,
    pub data: Vec<u8>,
    pub bss_size: u32,
    #[serde(borrow)]
    pub symbols: Vec<Symbol<'a>>,
    pub relocations: Vec<Relocation<'a>>,
}


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[derive(Serialize, Deserialize)]
pub struct Symbol<'a> {
    #[serde(borrow)]
    pub name: Identifier<'a>,
    pub section: Section,
    pub offset: u32,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[derive(Serialize, Deserialize)]
pub enum Section {
    Text,
    Data,
    Bss,
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[derive(Serialize, Deserialize)]
pub struct Relocation<'a> {
    #[serde(borrow)]
    pub arg: RelocArg<'a>,
    pub section: Section,
    pub offset: u32,
    pub data_format: RelocFormat,
    pub kind: RelocKind,
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[derive(Serialize, Deserialize)]
pub enum RelocArg<'a> {
    #[serde(borrow)]
    Symbol(Identifier<'a>),
    Constant(i32),
}


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[derive(Serialize, Deserialize)]
pub enum RelocFormat {
    B,
    C,
    D,
}
impl RelocFormat {
    pub fn encode_immediate(&self, immediate: i32) -> u32 {
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


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[derive(Serialize, Deserialize)]
pub struct RelocKind {
    pub pc_relative: bool,
    pub slice: RelocSlice,
}
impl Default for RelocKind {
    fn default() -> Self {
        Self {
            pc_relative: false,
            slice: RelocSlice::Whole,
        }
    }
}


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[derive(Serialize, Deserialize)]
pub enum RelocSlice {
    Whole,
    Low,
    High
}
