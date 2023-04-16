use std::collections::HashMap;
use super::{object_file::{ObjectFile, Section, Relocation, RelocArg, RelocSlice}, token::Identifier};


pub struct Linker<'a> {
    symbols: HashMap<Identifier<'a>, u32>,
    bytes: Vec<u8>,
    text_base: u32,
    data_base: u32,
    bss_base: u32,
}
impl<'a> Linker<'a> {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            bytes: Vec::new(),
            text_base: 0,
            data_base: 0,
            bss_base: 0,
        }
    }


    pub fn link(mut self, obj: &ObjectFile<'a>) -> Vec<u8> {
        self.load_text(obj);
        self.load_data(obj);
        self.load_bss();

        self.gen_symbol_values(obj);

        for reloc in &obj.relocations {
            self.apply_relocation(reloc);
        }


        self.bytes
    }
    fn load_text(&mut self, obj: &ObjectFile) {
        self.bytes.clear();
        self.text_base = 0;
        self.bytes.extend(&obj.text);
    }
    fn load_data(&mut self, obj: &ObjectFile) {
        self.data_base = self.bytes.len() as u32;
        self.bytes.extend(&obj.data);
    }
    fn load_bss(&mut self) {
        let mut address = self.bytes.len() as u32;
        while address % 4 != 0 {
            address += 1;
        }
        self.bss_base = address;
    }

    fn gen_symbol_values(&mut self, obj: &ObjectFile<'a>) {
        for sym in &obj.symbols {
            let address = self.base_address(sym.section) + sym.offset;
            self.symbols.insert(sym.name, address);
        }

        let bss_start = self.bss_base;
        let bss_end = bss_start + obj.bss_size;
        let bss_start_name = Identifier::global("__bss_start");
        let bss_end_name = Identifier::global("__bss_end");
        self.symbols.insert(bss_start_name, bss_start);
        self.symbols.insert(bss_end_name, bss_end);
    }


    fn apply_relocation(&mut self, reloc: &Relocation) {
        let mut value = match reloc.arg {
            RelocArg::Constant(val) => val,
            RelocArg::Symbol(name) => {
                let Some(&address) = self.symbols.get(&name) else { panic!("{name:?} is not a defined symbol") };
                i32::from_le_bytes(address.to_le_bytes())
            },
        };
        let address = self.base_address(reloc.section) + reloc.offset;

        if reloc.kind.pc_relative {
            value = value.wrapping_sub_unsigned(address);
        }

        match reloc.kind.slice {
            RelocSlice::Whole => (),
            RelocSlice::Low => value = get_low_high(value).0,
            RelocSlice::High => value = get_low_high(value).1,
        }

        let encoded = reloc.data_format.encode_immediate(value);
        let bytes = encoded.to_le_bytes();
        for (i, byte) in bytes.into_iter().enumerate() {
            self.bytes[address as usize + i] |= byte;
        }
    }


    fn base_address(&self, section: Section) -> u32 {
        match section {
            Section::Text => self.text_base,
            Section::Data => self.data_base,
            Section::Bss => self.bss_base,
        }
    }
}


fn round_nearest_multiple(value: i32, multiple: i32) -> i32 {
    let half = multiple / 2;
    let offset = if value.is_negative() { value - half } else { value + half };
    offset / 2 * 2
}
fn get_low_high(value: i32) -> (i32, i32) {
    let high = round_nearest_multiple(value, 4096) / 4096;
    let low = value - (high << 12);
    assert_eq!(low.wrapping_add(high << 12), value, "Your math is shit");

    (low, high)
}

