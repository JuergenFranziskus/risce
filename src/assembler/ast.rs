use super::{span::Span, token::Identifier};


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
    Equ(Expr<'a>),
    Op(Mnemonic, Vec<Arg<'a>>),
}
impl LineKind<'_> {
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::Empty => None,
            Self::Equ(val) => Some(val.span),
            Self::Op(_, args) => Some(args.last()?.span),
        }
    }
}


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Mnemonic {
    Mov,
    Lui,
    Jmp,
    Jal,
    Branch(Condition),
    Store,
    Load,
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


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Register(pub u8);


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


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr<'a> {
    pub span: Span,
    pub kind: ExprKind<'a>,
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind<'a> {
    Call(Function, Vec<Expr<'a>>),
    Paren(Box<Expr<'a>>),
    Identifier(Identifier<'a>),
    Decimal(&'a str),
    Hex(&'a str),
}


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Function {
    Low,
    High,
    Rel,
    Prep,
    Fin,
}
