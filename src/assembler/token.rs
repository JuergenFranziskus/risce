use super::span::Span;
use logos::{Lexer, Logos};

pub fn lex(src: &str) -> Vec<Token> {
    TokenKind::lexer(src)
        .spanned()
        .map(|(k, r)| Token {
            kind: k,
            span: Span::new(r.start, r.end - r.start),
        })
        .collect()
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Token<'a> {
    pub span: Span,
    pub kind: TokenKind<'a>,
}

#[derive(Logos, Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenKind<'a> {
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("$")]
    Dollar,
    #[token("@")]
    At,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("<<")]
    DoubleLess,

    #[token("section")]
    Section,
    #[token("resw")]
    ResW,
    #[token("equ")]
    Equ,
    #[token("db")]
    Db,

    #[token("byte")]
    Byte,
    #[token("short")]
    Short,
    #[token("word")]
    Word,

    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", split_identifier)]
    #[regex(r"\.[_a-zA-Z][_a-zA-Z0-9]*", split_identifier)]
    Identifier(Identifier<'a>),

    #[regex(r#""[^"]*""#)]
    StringLiteral(&'a str),

    #[regex(r"r[0-9]+|rsp|rbp")]
    Register(&'a str),

    #[regex(r"-?[0-9][_0-9]*")]
    Decimal(&'a str),

    #[regex(r"0x[0-9a-fA-F][_0-9a-fA-F]*")]
    Hex(&'a str),

    #[regex(r"\n")]
    Newline,

    #[error]
    #[regex(r"[ \t]+", logos::skip)]
    #[regex(r"#[^\n]*", logos::skip)]
    Error,
    
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier<'a> {
    pub name: &'a str,
    pub local: bool,
}
impl<'a> Identifier<'a> {
    pub fn new(name: &'a str, local: bool) -> Self {
        Self { name, local }
    }
    pub fn new_global(name: &'a str) -> Self {
        Self::new(name, false)
    }
    pub fn new_local(name: &'a str) -> Self {
        Self::new(name, true)
    }

    pub fn is_global(self) -> bool {
        !self.local
    }
    pub fn is_local(self) -> bool {
        self.local
    }
}

fn split_identifier<'a>(lexer: &'_ mut Lexer<'a, TokenKind<'a>>) -> Identifier<'a> {
    let src = lexer.slice();
    if let Some(local) = src.strip_prefix('.') {
        Identifier::new_local(local)
    } else {
        Identifier::new_global(src)
    }
}
