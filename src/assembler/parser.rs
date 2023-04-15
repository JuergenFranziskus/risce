use crate::assembler::ast::{LineKind, Condition};
use super::{token::{Token, TokenKind}, ast::{Ast, Line, Expr, Mnemonic, Arg, Register, ArgKind, MemArg, ExprKind, Function, MemSize, BinaryExpr, UnaryExpr, DBArg, DBArgKind}, span::Span};




pub struct Parser<'a, 'b> {
    tokens: &'b [Token<'a>],
    index: usize,
}
impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(tokens: &'b [Token<'a>]) -> Self {
        Self {
            tokens,
            index: 0,
        }
    }

    pub fn parse(mut self) -> Ast<'a> {
        let mut lines = Vec::new();
        while !self.at_end() {
            if let Some(line) = self.parse_line() {
                lines.push(line);
            }
        }


        Ast {
            lines,
        }
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



        if label != None || kind != LineKind::Empty {
            Some(Line {
                span: Span::merge(start, end),
                label,
                kind,
            })
        }
        else {
            None
        }
    }
    fn line_has_label(&self) -> bool {
        self.is_identifier() && (
            self.peek_is_token(1, TokenKind::Equ)
                || self.peek_is_token(1, TokenKind::Db)
                || self.peek_is_token(1, TokenKind::Colon)
        )
    }
    fn parse_equ(&mut self) -> LineKind<'a> {
        self.consume_token(TokenKind::Equ);
        let val = self.parse_expr();
        LineKind::Equ(val)
    }
    fn parse_db(&mut self) -> LineKind<'a> {
        self.consume_token(TokenKind::Db);
        let mut args = Vec::new();

        while !self.is_token(TokenKind::Newline) && !self.at_end() {
            args.push(self.parse_db_arg());
            if self.is_token(TokenKind::Comma) {
                self.next();
            }
            else {
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
    fn parse_op(&mut self) -> LineKind<'a> {
        let mnemonic = self.parse_mnemonic();
        let mut args = Vec::new();

        while !self.is_token(TokenKind::Newline) && !self.at_end() {
            args.push(self.parse_arg());
            if self.is_token(TokenKind::Comma) {
                self.next();
            }
            else {
                break;
            }
        }


        LineKind::Op(mnemonic, args)
    }
    fn parse_mnemonic(&mut self) -> Mnemonic {
        let TokenKind::Identifier(name) = self.curr().kind else { panic!("Expected mnemonic, found {:?}", self.curr()) };
        let Some(global) = name.global else { panic!("Could not extract mnemonic from {:?}", self.curr()) };
        assert!(name.local.is_none());
        self.next();

        match global {
            "mov" => Mnemonic::Mov,
            "lui" => Mnemonic::Lui,
            "jmp" => Mnemonic::Jmp,
            "jal" => Mnemonic::Jal,
            "call" => Mnemonic::Call,
            "ret" => Mnemonic::Ret,
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
            "enter" => Mnemonic::Enter,
            "leave" => Mnemonic::Leave,
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
            _ => (&name[1..]).parse().unwrap()
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
        }
        else {
            self.parse_expr_bp()
        }
    }
    fn try_parse_function(&mut self) -> Option<Function> {
        let TokenKind::Identifier(ident) = self.curr().kind else { return None };
        let Some(global) = ident.global else { return None };
        if ident.local.is_some() { return None };

        let func = match global {
            "low" => Function::Low,
            "high" => Function::High,
            "rel" => Function::Rel,
            "prep" => Function::Prep,
            "fin" => Function::Fin,
            _ => return None,
        };
        self.next();
        Some(func)
    }

    
    fn parse_expr_bp(&mut self) -> Expr<'a> {
        self.parse_expr_bp_prime(i16::MIN)
    }
    fn parse_expr_bp_prime(&mut self, min_bp: i16) -> Expr<'a> {
        let mut lhs = {
            let unary_op = match self.curr().kind {
                TokenKind::Percent => Some(UnaryExpr::Relative),
                _ => None,
            };
            if let Some(op) = unary_op {
                let start = self.curr().span;
                self.next();
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_expr_bp_prime(r_bp);
                let span = Span::merge(start, rhs.span);
                Expr {
                    span,
                    kind: ExprKind::Unary(op, rhs.into())
                }
            }
            else {
                self.parse_leaf_expr()
            }
        };
    
    
        while !self.at_end() {
            let op = match self.curr().kind {
                TokenKind::Plus => BinaryExpr::Add,
                TokenKind::Minus => BinaryExpr::Sub,
                TokenKind::Star => BinaryExpr::Mul,
                TokenKind::Slash => BinaryExpr::Div,
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
            TokenKind::Dollar => self.parse_here_expr(),
            _ => panic!("Cannot parse leaf expression at {:?}", self.curr()),
        }
    }
    fn parse_decimal_expr(&mut self) -> Expr<'a> {
        let span = self.curr().span;
        let TokenKind::Decimal(name) = self.curr().kind else { panic!() };
        self.next();
        Expr {
            span,
            kind: ExprKind::Decimal(name)
        }
    }
    fn parse_hex_expr(&mut self) -> Expr<'a> {
        let span = self.curr().span;
        let TokenKind::Hex(name) = self.curr().kind else { panic!() };
        self.next();
        Expr {
            span,
            kind: ExprKind::Hex(name)
        }
    }
    fn parse_ident_expr(&mut self) -> Expr<'a> {
        let span = self.curr().span;
        let TokenKind::Identifier(name) = self.curr().kind else { panic!() };
        self.next();
        Expr {
            span,
            kind: ExprKind::Identifier(name)
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
    fn parse_here_expr(&mut self) -> Expr<'a> {
        let span = self.curr().span;
        self.consume_token(TokenKind::Dollar);
        Expr {
            span,
            kind: ExprKind::Here
        }
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
        let Some(global) = name.global else { return false };
        if name.local.is_some() { return false };

        global == ident
    }
    fn consume_token(&mut self, kind: TokenKind) {
        assert!(self.is_token(kind), "Expected {kind:?}, found {:?}", self.try_curr());
        self.next();
    }
    fn is_identifier(&self) -> bool {
        matches!(self.try_curr().map(|t| t.kind), Some(TokenKind::Identifier(_)))
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


fn prefix_binding_power(op: UnaryExpr) -> ((), i16) {
    use UnaryExpr::*;
    match op {
        Relative => ((), 1001)
    }
}

fn infix_binding_power(op: BinaryExpr) -> (i16, i16) {
    use BinaryExpr::*;
    match op {
        Add | Sub => (0, 1),
        Mul | Div => (100, 101),
    }
}
