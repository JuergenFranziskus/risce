#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ObjectFile<'a> {
    pub symbols: Vec<Symbol<'a>>,
    pub relocations: Vec<Relocation<'a>>,
    pub text: Vec<u8>,
    pub data: Vec<u8>,
    pub bss_size: Calculation<'a>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct AbsoluteLabel<'a> {
    pub global: &'a str,
    pub local: Option<&'a str>,
}
impl<'a> From<&'a str> for AbsoluteLabel<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            global: value,
            local: None,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Section {
    Text,
    Data,
    Bss,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Symbol<'a> {
    pub name: AbsoluteLabel<'a>,
    pub value: Calculation<'a>,
    /// Whether this symbol is visible from other object files
    pub exported: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Relocation<'a> {
    pub address: Calculation<'a>,
    pub value: Calculation<'a>,
    pub format: RelocFormat,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum RelocFormat {
    Byte,
    Short,
    Word,
    IFormatB,
    IFormatC,
    IFormatD,
}

/// A list of operations to be applied in order to a stack of values.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Calculation<'a>(pub Vec<Operation<'a>>);
impl<'a> Calculation<'a> {
    pub fn start() -> Self {
        Self(Vec::new())
    }

    pub fn symbol(&mut self, name: impl Into<AbsoluteLabel<'a>>) {
        self.0.push(Operation::Symbol(name.into()));
    }
    pub fn constant(&mut self, value: u32) {
        self.0.push(Operation::Constant(value));
    }
    pub fn sconstant(&mut self, value: i32) {
        self.0.push(Operation::SConstant(value));
    }

    pub fn text_start(&mut self) {
        self.0.push(Operation::LocalTextSectionStart);
    }
    pub fn data_start(&mut self) {
        self.0.push(Operation::LocalDataSectionStart);
    }
    pub fn bss_start(&mut self) {
        self.0.push(Operation::LocalBssSectionStart);
    }

    pub fn make_relative(&mut self) {
        self.0.push(Operation::MakeRelative);
    }

    pub fn add(&mut self) {
        self.0.push(Operation::Add);
    }
    pub fn sub(&mut self) {
        self.0.push(Operation::Sub);
    }
    pub fn mul(&mut self) {
        self.0.push(Operation::Mul);
    }
    pub fn div(&mut self) {
        self.0.push(Operation::Div);
    }
    pub fn idiv(&mut self) {
        self.0.push(Operation::IDiv);
    }
    pub fn rem(&mut self) {
        self.0.push(Operation::Rem);
    }
    pub fn irem(&mut self) {
        self.0.push(Operation::IRem);
    }
    pub fn shl(&mut self) {
        self.0.push(Operation::Shl);
    }

    pub fn low(&mut self) {
        self.0.push(Operation::Low);
    }
    pub fn high(&mut self) {
        self.0.push(Operation::High);
    }

    pub fn global_bss_start(&mut self) {
        self.0.push(Operation::BssStart);
    }
    pub fn global_bss_end(&mut self) {
        self.0.push(Operation::BssEnd);
    }

    
}
impl<'a> Default for Calculation<'a> {
    fn default() -> Self {
        Self::start()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Operation<'a> {
    Symbol(AbsoluteLabel<'a>),
    Constant(u32),
    SConstant(i32),

    /// The starting address of the text section of the containing object file.
    LocalTextSectionStart,
    /// The starting address of the data section of the containing object file.
    LocalDataSectionStart,
    /// The starting address of the bss section of the containing object file.
    LocalBssSectionStart,

    BssStart,
    BssEnd,

    /// Only valid in relocations.
    /// Make the top value relative to the relocation's address.
    MakeRelative,

    /// Pop b. Pop a. Push a + b.
    Add,
    /// Pop b. Pop a. Push a - b.
    Sub,
    /// Pop b. Pop a. Push a * b.
    Mul,
    /// Pop b. Pop a. Push a / b.
    Div,
    /// Pop b. Pop a. Push a / b.
    IDiv,
    /// Pop b. Pop a. Push a % b.
    Rem,
    /// Pop b. Pop a. Push a % b.
    IRem,
    /// Pop b. Pop a. Push a << b.
    Shl,

    Low,
    High,
}
