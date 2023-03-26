use super::span::Span;
use logos::{Logos, Lexer};

pub fn lex(src: &str) -> Vec<Token> {
    TokenKind::lexer(src).spanned()
        .map(|(k, r)| Token { kind: k, span: Span::new(r.start, r.end - r.start) })
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
    #[token("+")]
    Plus,

    #[token("equ")]
    Equ,

    #[token("byte")]
    Byte,
    #[token("short")]
    Short,
    #[token("word")]
    Word,


    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", split_identifier)]
    #[regex(r"\.[_a-zA-Z0-9]+", split_identifier)]
    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*\.[_a-zA-Z0-9]+", split_identifier)]
    Identifier(Identifier<'a>),

    #[regex(r"r[0-9]+")]
    Register(&'a str),

    #[regex(r"[0-9][_0-9]*")]
    Decimal(&'a str),

    #[regex(r"0x[0-9a-fA-F][_0-9a-fA-F]*")]
    Hex(&'a str),

    #[regex(r"\n")]
    Newline,

    #[error]
    #[regex(r"[ \t]+", logos::skip)]
    Error,
}


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier<'a> {
    pub global: Option<&'a str>,
    pub local: Option<&'a str>,
}

fn split_identifier<'a>(lexer: &'_ mut Lexer<'a, TokenKind<'a>>) -> Identifier<'a> {
    let parts: Vec<_> = lexer.slice().split('.').collect();
    let mut global = None;
    let mut local  = None;
    if parts[0] != "" {
        global = Some(parts[0]);
    }
    if parts.len() == 2 && parts[1] != "" {
        local = Some(parts[1]);
    }

    Identifier {
        global,
        local
    }
}
