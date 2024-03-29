use super::{
    ast::{
        Arg, ArgKind, Ast, BinaryExpr, DBArg, DBArgKind, Expr, ExprKind, Function, Line, MemArg,
        MemSize, Mnemonic, Register,
    },
    span::Span,
    token::{Identifier, Token, TokenKind},
};
use crate::assembler::ast::{Condition, LineKind};

pub struct Parser<'a, 'b> {
    tokens: &'b [Token<'a>],
    index: usize,
}
impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(tokens: &'b [Token<'a>]) -> Self {
        Self { tokens, index: 0 }
    }

    pub fn parse(mut self) -> Ast<'a> {
        let mut lines = Vec::new();
        while !self.at_end() {
            if let Some(line) = self.parse_line() {
                lines.push(line);
            }
        }

        Ast { lines }
    }

    fn parse_line(&mut self) -> Option<Line<'a>> {
        let has_label = self.line_has_label();
        let mut label = None;
        let start = self.curr().span;
        let mut end = start;

        if has_label {
            let TokenKind::Identifier(name) = self.curr().kind else { unreachable!() };
            self.next();
            if self.is_token(TokenKind::Colon) {
                self.next();
            }
            label = Some(name);
        }

        let mut kind = LineKind::Empty;

        if !self.is_token(TokenKind::Newline) && !self.at_end() {
            kind = match self.curr().kind {
                TokenKind::Equ => self.parse_equ(),
                TokenKind::Db => self.parse_db(),
                TokenKind::ResW => self.parse_resw(),
                TokenKind::Section => self.parse_section(),
                _ => self.parse_op(),
            };
        }

        if let Some(span) = kind.span() {
            end = span;
        }

        if !self.at_end() {
            end = self.curr().span;
            self.consume_token(TokenKind::Newline);
        }

        if label.is_some() || kind != LineKind::Empty {
            Some(Line {
                span: Span::merge(start, end),
                label,
                kind,
            })
        } else {
            None
        }
    }
    fn line_has_label(&self) -> bool {
        self.is_identifier()
            && (self.peek_is_token(1, TokenKind::Equ)
                || self.peek_is_token(1, TokenKind::Db)
                || self.peek_is_token(1, TokenKind::Colon)
                || self.peek_is_token(1, TokenKind::ResW)
                || self.peek_is_token(1, TokenKind::Section))
    }
    fn parse_equ(&mut self) -> LineKind<'a> {
        self.consume_token(TokenKind::Equ);
        let name = self.parse_identifier();

        let val = self.parse_expr();
        LineKind::Equ(name, val)
    }
    fn parse_db(&mut self) -> LineKind<'a> {
        self.consume_token(TokenKind::Db);
        let mut args = Vec::new();

        while !self.is_token(TokenKind::Newline) && !self.at_end() {
            args.push(self.parse_db_arg());
            if self.is_token(TokenKind::Comma) {
                self.next();
            } else {
                break;
            }
        }

        LineKind::DB(args)
    }
    fn parse_db_arg(&mut self) -> DBArg<'a> {
        match self.curr().kind {
            TokenKind::StringLiteral(lit) => {
                let end = lit.len() - 1;
                let lit = &lit[1..end];
                let span = self.curr().span;
                self.next();
                DBArg {
                    span,
                    kind: DBArgKind::StringLiteral(lit),
                }
            }
            _ => {
                let e = self.parse_expr();
                let span = e.span;
                DBArg {
                    span,
                    kind: DBArgKind::Expression(e),
                }
            }
        }
    }
    fn parse_resw(&mut self) -> LineKind<'a> {
        self.consume_token(TokenKind::ResW);
        let bytes = self.parse_expr();
        LineKind::ResW(bytes)
    }
    fn parse_section(&mut self) -> LineKind<'a> {
        self.consume_token(TokenKind::Section);
        let name = self.parse_identifier();
        assert!(name.is_global());
        LineKind::Section(name.name)
    }
    fn parse_op(&mut self) -> LineKind<'a> {
        let mnemonic = self.parse_mnemonic();
        let mut args = Vec::new();

        while !self.is_token(TokenKind::Newline) && !self.at_end() {
            args.push(self.parse_arg());
            if self.is_token(TokenKind::Comma) {
                self.next();
            } else {
                break;
            }
        }

        LineKind::Op(mnemonic, args)
    }
    fn parse_mnemonic(&mut self) -> Mnemonic {
        let TokenKind::Identifier(name) = self.curr().kind else { panic!("Expected mnemonic, found {:?}", self.curr()) };
        assert!(
            name.is_global(),
            "{name:?} is not a valid mnemonic as it is not a global identifier"
        );
        self.next();

        match name.name {
            "nop" => Mnemonic::Nop,
            "mov" => Mnemonic::Mov,
            "lui" => Mnemonic::Lui,
            "jmp" => Mnemonic::Jmp,
            "djmp" => Mnemonic::DJmp,
            "jal" => Mnemonic::Jal,
            "djal" => Mnemonic::DJal,
            "call" => Mnemonic::Call,
            "dcall" => Mnemonic::DCall,
            "ret" => Mnemonic::Ret,
            "dret" => Mnemonic::DRet,
            "jeq" => Mnemonic::Branch(Condition::Equal),
            "jne" => Mnemonic::Branch(Condition::NotEqual),
            "jgt" => Mnemonic::Branch(Condition::Greater),
            "jlt" => Mnemonic::Branch(Condition::Less),
            "ja" => Mnemonic::Branch(Condition::Above),
            "jb" => Mnemonic::Branch(Condition::Below),
            "jng" => Mnemonic::Branch(Condition::NotGreater),
            "jnl" => Mnemonic::Branch(Condition::NotLess),
            "jna" => Mnemonic::Branch(Condition::NotAbove),
            "jnb" => Mnemonic::Branch(Condition::NotBelow),

            "djeq" => Mnemonic::DBranch(Condition::Equal),
            "djne" => Mnemonic::DBranch(Condition::NotEqual),
            "djgt" => Mnemonic::DBranch(Condition::Greater),
            "djlt" => Mnemonic::DBranch(Condition::Less),
            "dja" =>  Mnemonic::DBranch(Condition::Above),
            "djb" =>  Mnemonic::DBranch(Condition::Below),
            "djng" => Mnemonic::DBranch(Condition::NotGreater),
            "djnl" => Mnemonic::DBranch(Condition::NotLess),
            "djna" => Mnemonic::DBranch(Condition::NotAbove),
            "djnb" => Mnemonic::DBranch(Condition::NotBelow),

            "store" => Mnemonic::Store,
            "load" => Mnemonic::Load,
            "lea" => Mnemonic::Lea,
            "not" => Mnemonic::Not,
            "neg" => Mnemonic::Neg,
            "add" => Mnemonic::Add,
            "sub" => Mnemonic::Sub,
            "and" => Mnemonic::And,
            "or" => Mnemonic::Or,
            "nand" => Mnemonic::Nand,
            "xor" => Mnemonic::Xor,
            "shl" => Mnemonic::Shl,
            "shr" => Mnemonic::Shr,
            "sar" => Mnemonic::Sar,
            "rol" => Mnemonic::Rol,
            "ror" => Mnemonic::Ror,
            "mul" => Mnemonic::Mul,
            "umul" => Mnemonic::UMul,
            "imul" => Mnemonic::IMul,
            "udiv" => Mnemonic::UDiv,
            "idiv" => Mnemonic::IDiv,
            "urem" => Mnemonic::URem,
            "irem" => Mnemonic::IRem,
            "seq" => Mnemonic::Set(Condition::Equal),
            "sne" => Mnemonic::Set(Condition::NotEqual),
            "sgt" => Mnemonic::Set(Condition::Greater),
            "slt" => Mnemonic::Set(Condition::Less),
            "sa" => Mnemonic::Set(Condition::Above),
            "sb" => Mnemonic::Set(Condition::Below),
            "sng" => Mnemonic::Set(Condition::NotGreater),
            "snl" => Mnemonic::Set(Condition::NotLess),
            "sna" => Mnemonic::Set(Condition::NotAbove),
            "snb" => Mnemonic::Set(Condition::NotBelow),
            "ceq" => Mnemonic::Choose(Condition::Equal),
            "cne" => Mnemonic::Choose(Condition::NotEqual),
            "cgt" => Mnemonic::Choose(Condition::Greater),
            "clt" => Mnemonic::Choose(Condition::Less),
            "ca" =>  Mnemonic::Choose(Condition::Above),
            "cb" =>  Mnemonic::Choose(Condition::Below),
            "cng" => Mnemonic::Choose(Condition::NotGreater),
            "cnl" => Mnemonic::Choose(Condition::NotLess),
            "cna" => Mnemonic::Choose(Condition::NotAbove),
            "cnb" => Mnemonic::Choose(Condition::NotBelow),
            "enter" => Mnemonic::Enter,
            "leave" => Mnemonic::Leave,
            "exit" => Mnemonic::Exit,
            a => panic!("{a} is not an instruction mnemonic"),
        }
    }
    fn parse_arg(&mut self) -> Arg<'a> {
        let start = self.curr().span;
        let mut end = start;

        use TokenKind::*;
        let kind = match self.curr().kind {
            Register(_) => ArgKind::Register(self.parse_register()),
            OpenBracket | Byte | Short | Word => ArgKind::Memory(self.parse_memory_arg()),
            _ => ArgKind::Expression(self.parse_expr()),
        };

        if let Some(span) = kind.span() {
            end = span;
        }

        Arg {
            span: Span::merge(start, end),
            kind,
        }
    }

    fn parse_register(&mut self) -> Register {
        let TokenKind::Register(name) = self.curr().kind else { panic!() };
        self.next();

        let id = match name {
            "rsp" => 30,
            "rbp" => 29,
            _ => name[1..].parse().unwrap(),
        };
        Register(id)
    }
    fn parse_memory_arg(&mut self) -> MemArg<'a> {
        let start = self.curr().span;
        let mut rip_relative = false;
        let mut base = None;
        let mut offset = None;
        let size = self.parse_optional_mem_size();

        self.consume_token(TokenKind::OpenBracket);
        if self.is_global_identifier("rip") {
            rip_relative = true;
            self.next();
            self.consume_token(TokenKind::Plus);
        }

        if let TokenKind::Register(_) = self.curr().kind {
            base = Some(self.parse_register());
            if !self.is_token(TokenKind::CloseBracket) {
                self.consume_token(TokenKind::Plus);
            }
        }

        if !self.is_token(TokenKind::CloseBracket) {
            offset = Some(self.parse_expr());
        }

        let end = self.curr().span;
        self.consume_token(TokenKind::CloseBracket);

        MemArg {
            span: Span::merge(start, end),
            rip_relative,
            base,
            offset,
            size,
        }
    }
    fn parse_optional_mem_size(&mut self) -> Option<MemSize> {
        let size = match self.curr().kind {
            TokenKind::Byte => MemSize::Byte,
            TokenKind::Short => MemSize::Short,
            TokenKind::Word => MemSize::Word,
            _ => return None,
        };
        self.next();
        Some(size)
    }

    fn parse_expr(&mut self) -> Expr<'a> {
        self.parse_applicative_expr()
    }

    fn parse_applicative_expr(&mut self) -> Expr<'a> {
        let start = self.curr().span;
        if let Some(function) = self.try_parse_function() {
            let mut end = start;
            let mut args = Vec::new();
            while !self.ends_applicative_expr() {
                let arg = self.parse_expr_bp();
                end = arg.span;
                args.push(arg);
            }

            Expr {
                span: Span::merge(start, end),
                kind: ExprKind::Call(function, args),
            }
        } else {
            self.parse_expr_bp()
        }
    }
    fn try_parse_function(&mut self) -> Option<Function> {
        let TokenKind::Identifier(ident) = self.curr().kind else { return None };
        if ident.is_local() {
            return None;
        };
        let func = match ident.name {
            "low" => Function::Low,
            "high" => Function::High,
            "udiv" => Function::UDiv,
            "idiv" => Function::IDiv,
            _ => return None,
        };
        self.next();
        Some(func)
    }

    fn parse_expr_bp(&mut self) -> Expr<'a> {
        self.parse_expr_bp_prime(i16::MIN)
    }
    fn parse_expr_bp_prime(&mut self, min_bp: i16) -> Expr<'a> {
        let mut lhs = self.parse_leaf_expr();

        while !self.at_end() {
            let op = match self.curr().kind {
                TokenKind::Plus => BinaryExpr::Add,
                TokenKind::Minus => BinaryExpr::Sub,
                TokenKind::Star => BinaryExpr::Mul,
                TokenKind::DoubleLess => BinaryExpr::Shl,
                _ => break,
            };

            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }
            self.next();
            let rhs = self.parse_expr_bp_prime(r_bp);
            let span = Span::merge(lhs.span, rhs.span);
            lhs = Expr {
                span,
                kind: ExprKind::Binary(op, lhs.into(), rhs.into()),
            }
        }

        lhs
    }

    fn parse_leaf_expr(&mut self) -> Expr<'a> {
        match self.curr().kind {
            TokenKind::Decimal(_) => self.parse_decimal_expr(),
            TokenKind::Hex(_) => self.parse_hex_expr(),
            TokenKind::Identifier(_) => self.parse_ident_expr(),
            TokenKind::OpenParen => self.parse_paren_expr(),
            _ => panic!("Cannot parse leaf expression at {:?}", self.curr()),
        }
    }
    fn parse_decimal_expr(&mut self) -> Expr<'a> {
        let span = self.curr().span;
        let TokenKind::Decimal(name) = self.curr().kind else { panic!() };
        self.next();
        Expr {
            span,
            kind: ExprKind::Decimal(name),
        }
    }
    fn parse_hex_expr(&mut self) -> Expr<'a> {
        let span = self.curr().span;
        let TokenKind::Hex(name) = self.curr().kind else { panic!() };
        self.next();
        Expr {
            span,
            kind: ExprKind::Hex(name),
        }
    }
    fn parse_ident_expr(&mut self) -> Expr<'a> {
        let span = self.curr().span;
        let name = self.parse_identifier();

        Expr {
            span,
            kind: ExprKind::Identifier(name),
        }
    }
    fn parse_paren_expr(&mut self) -> Expr<'a> {
        let start = self.curr().span;
        self.consume_token(TokenKind::OpenParen);

        let value = self.parse_expr();

        let end = self.curr().span;
        self.consume_token(TokenKind::CloseParen);

        Expr {
            span: Span::merge(start, end),
            kind: ExprKind::Paren(Box::new(value)),
        }
    }

    fn parse_identifier(&mut self) -> Identifier<'a> {
        let TokenKind::Identifier(name) = self.curr().kind else { panic!() };
        self.next();
        name
    }

    fn ends_applicative_expr(&self) -> bool {
        self.is_token(TokenKind::CloseParen)
            || self.is_token(TokenKind::CloseBracket)
            || self.is_token(TokenKind::Comma)
            || self.is_token(TokenKind::Newline)
            || self.at_end()
    }
    fn is_global_identifier(&mut self, ident: &str) -> bool {
        let TokenKind::Identifier(name) = self.curr().kind else { return false };
        name.is_global() && name.name == ident
    }
    fn consume_token(&mut self, kind: TokenKind) -> Span {
        assert!(
            self.is_token(kind),
            "Expected {kind:?}, found {:?}",
            self.try_curr()
        );
        let span = self.curr().span;
        self.next();
        span
    }
    fn is_identifier(&self) -> bool {
        matches!(
            self.try_curr().map(|t| t.kind),
            Some(TokenKind::Identifier(_))
        )
    }
    fn is_token(&self, kind: TokenKind) -> bool {
        self.try_curr().map(|t| t.kind) == Some(kind)
    }
    fn peek_is_token(&self, offset: usize, kind: TokenKind) -> bool {
        self.try_peek(offset).map(|t| t.kind) == Some(kind)
    }
    fn at_end(&self) -> bool {
        self.index >= self.tokens.len()
    }
    fn next(&mut self) {
        self.index += 1;
    }
    fn curr(&self) -> Token<'a> {
        self.tokens[self.index]
    }
    fn try_curr(&self) -> Option<Token<'a>> {
        self.tokens.get(self.index).copied()
    }
    fn try_peek(&self, offset: usize) -> Option<Token<'a>> {
        let index = self.index + offset;
        self.tokens.get(index).copied()
    }
}

fn infix_binding_power(op: BinaryExpr) -> (i16, i16) {
    use BinaryExpr::*;
    match op {
        Shl => (-100, -99),
        Add | Sub => (0, 1),
        Mul => (100, 101),
    }
}
