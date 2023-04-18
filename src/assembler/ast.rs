use super::{span::Span, token::Identifier, object_file::{RelocKind, RelocSlice}};


#[derive(Clone, Debug)]
pub struct Ast<'a> {
    pub lines: Vec<Line<'a>>,
}

#[derive(Clone, Debug)]
pub struct Line<'a> {
    pub span: Span,
    pub label: Option<Identifier<'a>>,
    pub kind: LineKind<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LineKind<'a> {
    Empty,
    Equ(&'a str, Expr<'a>),
    Op(Mnemonic, Vec<Arg<'a>>),
    DB(Vec<DBArg<'a>>),
    ResW(Expr<'a>),
    Section(&'a str),
}
impl LineKind<'_> {
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::Empty => None,
            Self::Equ(_, val) => Some(val.span),
            Self::Op(_, args) => Some(args.last()?.span),
            Self::DB(args) => Some(args.last()?.span),
            Self::ResW(e) => Some(e.span),
            Self::Section(_) => None,
        }
    }
}


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Mnemonic {
    Mov,
    Lui,
    Jmp,
    Jal,
    Call,
    Ret,
    Branch(Condition),
    Store,
    Load,
    Lea,
    Not,
    Neg,
    Add,
    Sub,
    And,
    Or,
    Nand,
    Xor,
    Shl,
    Shr,
    Sar,
    Rol,
    Ror,
    Mul,
    UMul,
    IMul,
    UDiv,
    URem,
    IDiv,
    IRem,
    Set(Condition),

    Enter,
    Leave,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Condition {
    Greater,
    Less,
    Above,
    Below,
    NotGreater,
    NotLess,
    NotAbove,
    NotBelow,
    Equal,
    NotEqual,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Arg<'a> {
    pub span: Span,
    pub kind: ArgKind<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArgKind<'a> {
    Register(Register),
    Expression(Expr<'a>),
    Memory(MemArg<'a>),
}
impl ArgKind<'_> {
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::Register(_) => None,
            Self::Expression(val) => Some(val.span),
            Self::Memory(arg) => Some(arg.span),
        }
    }
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DBArg<'a> {
    pub span: Span,
    pub kind: DBArgKind<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DBArgKind<'a> {
    Expression(Expr<'a>),
    StringLiteral(&'a str),
}



#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Register(pub u8);
impl Register {
    pub fn rsp() -> Register {
        Register(30)
    }
    pub fn rbp() -> Register {
        Register(29)
    }
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemArg<'a> {
    pub span: Span,
    pub rip_relative: bool,
    pub base: Option<Register>,
    pub offset: Option<Expr<'a>>,
    pub size: Option<MemSize>,
}


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum MemSize {
    Byte,
    Short,
    Word,
}
impl MemSize {
    pub fn to_store_bits(self) -> u32 {
        let size = match self {
            Self::Byte => 0,
            Self::Short => 1,
            Self::Word => 3,
        };
        size << 22
    }
    pub fn to_load_bits(self) -> u32 {
        let size = match self {
            Self::Byte => 0,
            Self::Short => 1,
            Self::Word => 3,
        };
        size << 17
    }
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr<'a> {
    pub span: Span,
    pub kind: ExprKind<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind<'a> {
    Binary(BinaryExpr, Box<Expr<'a>>, Box<Expr<'a>>),
    Call(Function, Vec<Expr<'a>>),
    Paren(Box<Expr<'a>>),
    Identifier(Identifier<'a>, Option<Relocation>),
    Decimal(&'a str),
    Hex(&'a str),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinaryExpr {
    Add,
    Sub,
    Mul,
    Div,
}


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Function {
    Low,
    High,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Relocation {
    REL,
    LOW,
    HIGH,
}
impl Relocation {
    pub fn to_reloc_kind(self) -> RelocKind {
        let (pc_relative, slice) = match self {
            Self::REL => (true, RelocSlice::Whole),
            Self::LOW => (false, RelocSlice::Low),
            Self::HIGH => (false, RelocSlice::High),
        };

        RelocKind {
            pc_relative,
            slice,
        }
    }
}
