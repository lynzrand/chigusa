use super::ast::Literal;
use super::ast::*;
use super::err::*;
use super::lexer::*;
use crate::prelude::*;
use bimap::BiMap;
use std::iter::Iterator;
use std::iter::Peekable;

use either::Either;
use LoopCtrl::*;

pub trait IntoParser<T>
where
    T: Iterator<Item = Token>,
{
    fn into_parser(self) -> Parser<T>;
}

impl<L> IntoParser<Lexer<L>> for Lexer<L>
where
    L: Iterator<Item = char>,
{
    fn into_parser(self) -> Parser<Lexer<L>> {
        Parser::new(self)
    }
}

// pub trait TokenIterator: Iterator<Item = Token> + itertools::PeekingNext {
//     fn expect(&mut self, token: TokenVariant) -> ParseResult<Token> {
//         // * separated variables because lifetime concerns.
//         match self.next() {
//             Some(t) => {
//                 if variant_eq(&t.var, &token) {
//                     Ok(t)
//                 } else {
//                     Err(parse_err(ParseErrVariant::ExpectToken(token), t.span))
//                 }
//             }
//             None => Err(parse_err(ParseErrVariant::ExpectToken(token), Span::zero())),
//         }
//     }

//     fn expect_peek(&mut self, token: TokenVariant) -> ParseResult<Token> {
//         // * separated variables because lifetime concerns.
//         match self.peeking_next(|_| false) {
//             Some(t) => {
//                 if variant_eq(&t.var, &token) {
//                     Ok(t)
//                 } else {
//                     Err(parse_err(ParseErrVariant::ExpectToken(token), t.span))
//                 }
//             }
//             None => Err(parse_err(ParseErrVariant::ExpectToken(token), Span::zero())),
//         }
//     }

//     fn expect_map_or<T>(
//         &mut self,
//         token: TokenVariant,
//         map: impl FnOnce(Token) -> T,
//         f: impl FnOnce(Token) -> Result<T, ParseError>,
//     ) -> ParseResult<T> {
//         let next = self.next();
//         match next {
//             Some(v) => {
//                 if variant_eq(&v.var, &token) {
//                     Ok(map(v))
//                 } else {
//                     f(v)
//                 }
//             }
//             None => Err(parse_err(ParseErrVariant::ExpectToken(token), Span::zero())),
//         }
//     }

//     fn try_consume(&mut self, token: TokenVariant) -> bool {
//         match self.peeking_next(|v| variant_eq(&v.var, &token)) {
//             Some(_) => true,
//             None => false,
//         }
//     }

//     fn try_consume_log_span(&mut self, token: TokenVariant) -> Option<Span> {
//         match self.peeking_next(|v| variant_eq(&v.var, &token)) {
//             Some(v) => Some(v.span),
//             None => None,
//         }
//     }
// }

// type LexerWrapped = Peekable<Lexer>;

// impl TokenIterator for LexerWrapped {}

pub struct TypeVar {
    types: Vec<TypeDef>,
    type_names: BiMap<usize, String>,
    // vars: Vec<VarDef>,
    // var_names: BiMap<usize, String>,
}

impl TypeVar {
    pub fn new() -> TypeVar {
        TypeVar {
            types: Vec::new(),
            type_names: BiMap::new(),
            // vars: Vec::new(),
            // var_names: BiMap::new(),
        }
    }

    pub fn insert_type(&mut self, type_name: &str, type_def: TypeDef) -> usize {
        unimplemented!()
    }
}

pub struct Parser<T>
where
    T: Iterator<Item = Token>,
{
    lexer: T,
    type_var: TypeVar,
    cur: Token,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(mut lexer: T) -> Parser<T> {
        log::trace!("Created a new parser.");

        let mut parser = Parser {
            lexer: lexer,
            type_var: TypeVar::new(),
            cur: Token::dummy(),
        };
        parser.cur = parser.lexer.next().unwrap();
        parser
    }

    fn bump(&mut self) -> Token {
        let mut next = self.lexer.next().unwrap_or_else(|| Token::eof());
        std::mem::swap(&mut self.cur, &mut next);

        log::trace!("Switching to next token; is: {:#}", self.cur);
        next
    }

    fn check(&self, accept: &TokenType) -> bool {
        variant_eq(&self.cur.var, accept)
    }

    fn expect(&mut self, accept: &TokenType) -> bool {
        if self.check(accept) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn check_report(&mut self, accept: &TokenType) -> ParseResult<()> {
        if self.check(accept) {
            Ok(())
        } else {
            Err(parse_err(
                // We used clone here, because once we meet an error we no longer
                // need to worry about performance. Things're gonna fail anyway.
                ParseErrVariant::ExpectToken(accept.clone()),
                self.cur.span,
            ))
        }
    }

    fn expect_report(&mut self, accept: &TokenType) -> ParseResult<()> {
        if self.expect(accept) {
            Ok(())
        } else {
            Err(parse_err(
                // We used clone here, because once we meet an error we no longer
                // need to worry about performance. Things're gonna fail anyway.
                ParseErrVariant::ExpectToken(accept.clone()),
                self.cur.span,
            ))
        }
    }

    fn check_one_of(&mut self, accept: &[TokenType]) -> bool {
        accept.iter().any(|x| variant_eq(&self.cur.var, &x))
    }

    fn expect_one_of(&mut self, accept: &[TokenType]) -> bool {
        if self.check_one_of(accept) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn check_one_of_report(&mut self, accept: &[TokenType]) -> ParseResult<()> {
        if self.check_one_of(accept) {
            Ok(())
        } else {
            Err(parse_err(
                // We used clone here, because once we meet an error we no longer
                // need to worry about performance. Things're gonna fail anyway.
                ParseErrVariant::ExpectTokenOneOf(accept.iter().map(|x| x.clone()).collect()),
                self.cur.span,
            ))
        }
    }

    fn expect_one_of_report(&mut self, accept: &[TokenType]) -> ParseResult<()> {
        if self.expect_one_of(accept) {
            Ok(())
        } else {
            Err(parse_err(
                // We used clone here, because once we meet an error we no longer
                // need to worry about performance. Things're gonna fail anyway.
                ParseErrVariant::ExpectTokenOneOf(accept.iter().map(|x| x.clone()).collect()),
                self.cur.span,
            ))
        }
    }

    pub fn parse(&mut self) -> ParseResult<Program> {
        self.p_program()
    }

    fn inject_std(scope: Ptr<Scope>) {
        let mut scope = scope.borrow_mut();
        scope
            .insert_def(
                "int",
                SymbolDef::Typ {
                    def: Ptr::new(TypeDef::Primitive(PrimitiveType {
                        var: PrimitiveTypeVar::SignedInt,
                        occupy_bytes: 4,
                    })),
                },
            )
            .expect("Failed to inject primitive type `int`");
    }

    fn p_program(&mut self) -> ParseResult<Program> {
        let root_scope = Ptr::new(Scope::new());
        Self::inject_std(root_scope.clone());
        let mut stmts = Vec::new();
        while self.cur.var != TokenType::EndOfFile {
            stmts.push(self.p_stmt(root_scope.clone())?)
        }
        Ok(Program {
            scope: root_scope,
            vars: todo!(),
            types: todo!(),
        })
    }

    fn p_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        match &self.cur.var {
            TokenType::LCurlyBrace => self.p_block_stmt(scope),
            TokenType::Identifier(..) => self.p_decl_or_expr(scope),
            TokenType::If => self.p_if_stmt(scope),
            TokenType::While => self.p_while_stmt(scope),
            TokenType::Const => self.p_decl_stmt(scope),
            TokenType::LParenthesis
            | TokenType::LBracket
            | TokenType::Literal(..)
            | TokenType::Multiply
            | TokenType::Increase
            | TokenType::Decrease => self.p_expr_stmt(scope),
            _ => todo!(),
        }
    }

    fn p_decl_or_expr(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        match &self.cur.var {
            TokenType::Identifier(ident) => {
                let entry = scope.borrow().find_def(ident);
                match entry {
                    None => Err(parse_err(
                        ParseErrVariant::CannotFindIdent(ident.into()),
                        self.cur.span,
                    )),
                    Some(entry) => {
                        let entry = entry.borrow();
                        match &*entry {
                            &SymbolDef::Typ { .. } => self.p_decl_stmt(scope.clone()),
                            &SymbolDef::Var { .. } => self.p_expr_stmt(scope.clone()),
                        }
                    }
                }
            }

            // Statements starting with bracket `[` are always declarations (of arrays)
            TokenType::LBracket => self.p_decl_stmt(scope),

            // Statements starting with `++`, `--`, `*` and `(` are always expressions
            TokenType::Increase
            | TokenType::Decrease
            | TokenType::Multiply
            | TokenType::LParenthesis => self.p_expr_stmt(scope),

            _ => unreachable!(),
        }
    }

    fn p_block_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        let new_scope = Ptr::new(Scope::new_with_parent(scope));
        self.p_block_stmt_no_scope(new_scope)
    }

    fn p_block_stmt_no_scope(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        let l_span = self.cur.span;
        self.expect_report(&TokenType::LCurlyBrace)?;
        let mut stmts = Vec::new();

        // For each statement, parse
        while !self.check(&TokenType::RCurlyBrace) {
            let stmt = self.p_stmt(scope.clone())?;
            stmts.push(stmt);
        }

        let r_span = self.cur.span;
        self.expect_report(&TokenType::RCurlyBrace)?;
        Ok(Stmt {
            var: StmtVariant::Block(Block { scope, stmts }),
            span: l_span + r_span,
        })
    }

    fn p_type_name(&mut self, scope: Ptr<Scope>) -> ParseResult<Ptr<TypeDef>> {
        let tok = self.bump();
        match tok.var {
            TokenType::BinaryAnd => Ok(Ptr::new(TypeDef::Ref(RefType {
                target: self.p_type_name(scope)?,
            }))),
            TokenType::LBracket => {
                let typ = TypeDef::Array(ArrayType {
                    target: self.p_type_name(scope)?,
                    length: None,
                });
                self.expect_report(&TokenType::RBracket)?;
                Ok(Ptr::new(typ))
            }
            TokenType::Identifier(ident) => {
                let span = tok.span;
                match scope.borrow().find_def(&ident) {
                    None => Err(parse_err(
                        ParseErrVariant::CannotFindType(ident.into()),
                        span,
                    )),
                    Some(def) => match &*def.borrow() {
                        // TODO: Add generics?
                        SymbolDef::Typ { .. } => Ok(Ptr::new(TypeDef::NamedType(ident.into()))),
                        _ => Err(parse_err(
                            ParseErrVariant::CannotFindType(ident.into()),
                            span,
                        )),
                    },
                }
            }
            _ => unreachable!(),
        }
    }

    fn p_fn(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        unimplemented!()
    }

    fn p_decl_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        // This is the identifier token
        let init_span = self.cur.span;
        let is_const = self.expect(&TokenType::Const);
        let typeDecl = self.p_type_name(scope.clone())?;
        let mut has_next = true;
        let mut exprs = Vec::new();

        while has_next {
            self.check_report(&TokenType::Identifier(String::new()))?;
            let ident = self.bump();
            let init_val = if self.expect(&TokenType::Assign) {
                Some(self.p_base_expr(&[TokenType::Comma, TokenType::Semicolon], scope.clone())?)
            } else {
                None
            };
            scope.borrow_mut().insert_def(
                ident.get_ident().unwrap(),
                SymbolDef::Var {
                    typ: typeDecl.clone(),
                    is_const,
                    is_callable: false,
                },
            )?;

            if let Some(val) = init_val {
                let span = ident.span + val.borrow().span();
                exprs.push(Ptr::new(Expr {
                    var: ExprVariant::BinaryOp(BinaryOp {
                        op: if is_const { OpVar::_Csn } else { OpVar::_Asn },
                        lhs: Ptr::new(Expr {
                            var: ExprVariant::Ident(Identifier {
                                name: ident.get_ident().unwrap().into(),
                            }),
                            span: ident.span,
                        }),
                        rhs: val,
                    }),
                    span,
                }))
            }

            has_next = self.expect(&TokenType::Comma);
        }

        let span = exprs
            .iter()
            .fold(init_span, |last, this| this.borrow().span() + last);

        self.expect_report(&TokenType::Semicolon)?;
        Ok(Stmt {
            var: StmtVariant::ManyExpr(exprs),
            span,
        })
    }

    fn p_while_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        unimplemented!()
    }

    fn p_if_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        let mut span = self.cur.span;

        self.expect_report(&TokenType::If)?;
        todo!("We are refactoring this thing")
    }

    fn p_expr_stmt(&mut self, scope: Ptr<Scope>) -> ParseResult<Stmt> {
        // TODO: Subject to change
        let expr = self.p_base_expr(&[TokenType::Semicolon], scope)?;
        let span = expr.borrow().span();
        self.expect_report(&TokenType::Semicolon)?;
        Ok(Stmt {
            var: StmtVariant::Expr(expr),
            span,
        })
    }

    fn p_base_expr(
        &mut self,
        close_delim: &[TokenType],
        scope: Ptr<Scope>,
    ) -> ParseResult<Ptr<Expr>> {
        let mut expr = None;
        while !self.check_one_of(close_delim) {
            expr = Some(self.p_binary_op(expr, 0, scope.clone())?);
        }
        expr.ok_or_else(|| {
            parse_err_z(ParseErrVariant::InternalErr(
                "Invalid branching into expression parsing".into(),
            ))
        })
    }

    /// Parses a binary operator with at least the precedence specified.
    ///
    /// Design stolen from https://github.com/rust-lang/rust/blob/b5f265eeed23ac87ec6b4a7e6bc7cb4ea3e67c31/src/librustc_parse/parser/expr.rs#L141
    fn p_binary_op(
        &mut self,
        lhs: Option<Ptr<Expr>>,
        expect_prec: isize,
        scope: Ptr<Scope>,
    ) -> ParseResult<Ptr<Expr>> {
        let lhs = if lhs.is_some() {
            lhs.unwrap()
        } else {
            self.p_prefix_unary_op(scope.clone())?
        };

        // Op should be self.cur
        if let Some(op) = self.cur.var.into_op(false, false) {
            if (op.is_left_associative() && op.priority() > expect_prec)
                || (op.is_right_associative() && op.priority() >= expect_prec)
            {
                self.bump();
                let rhs = self.p_binary_op(None, op.priority(), scope.clone())?;
                let span = { lhs.borrow().span() + rhs.borrow().span() };
                Ok(Ptr::new(Expr {
                    var: ExprVariant::BinaryOp(BinaryOp { lhs, rhs, op }),
                    span,
                }))
            } else {
                Ok(lhs)
            }
        } else {
            Ok(lhs)
        }
    }

    fn p_prefix_unary_op(&mut self, scope: Ptr<Scope>) -> ParseResult<Ptr<Expr>> {
        let mut op_vec = Vec::new();
        while let Some(op) = self.cur.var.into_op(true, false) {
            op_vec.push((op, self.cur.span));
            self.bump();
        }
        let mut expr = self.p_postfix_unary_op(scope)?;
        while let Some((op, span)) = op_vec.pop() {
            let span = span + expr.borrow().span();
            expr = Ptr::new(Expr {
                var: ExprVariant::UnaryOp(UnaryOp { op, val: expr }),
                span,
            });
        }
        Ok(expr)
    }

    fn p_postfix_unary_op(&mut self, scope: Ptr<Scope>) -> ParseResult<Ptr<Expr>> {
        let mut expr = self.p_item(scope)?;
        loop {
            if let Some(op) = self.cur.var.into_op(false, true) {
                expr = Ptr::new(Expr {
                    var: ExprVariant::UnaryOp(UnaryOp { op, val: expr }),
                    span: self.cur.span,
                });
                self.bump();
            } else if self.cur.var == TokenType::LBracket {
                // Parse index operator
                todo!("Parse index operator");
            } else if self.cur.var == TokenType::Dot {
                // Parse child operator
                todo!("Parse child operator");
            } else {
                // There's no postfix unary operator for us to parse
                break;
            }
        }
        Ok(expr)
    }

    /// Parse an item in expression
    ///
    /// An item is either a expression wrapped in parentheses, or an identifier,
    /// or a literal value.
    fn p_item(&mut self, scope: Ptr<Scope>) -> ParseResult<Ptr<Expr>> {
        if self.cur.var == TokenType::LParenthesis {
            // Start a new parsing cycle!
            let expr = self.p_base_expr(&[TokenType::RParenthesis], scope);
            self.expect_report(&TokenType::RParenthesis)?;
            expr
        } else {
            if self.check(&TokenType::Literal(unsafe {
                std::mem::MaybeUninit::uninit().assume_init()
            })) {
                self.p_literal()
            } else if self.check(&TokenType::Identifier(String::new())) {
                self.p_ident_or_fn(scope)
            } else {
                Err(parse_err(
                    ParseErrVariant::ExpectTokenOneOf(vec![
                        TokenType::Literal(unsafe {
                            std::mem::MaybeUninit::uninit().assume_init()
                        }),
                        TokenType::Identifier(String::new()),
                    ]),
                    self.cur.span,
                ))
            }
        }
    }

    /// Parse an identifier or function call.
    ///
    /// This parser accepts a starting state when `self.cur` is the first `Identifier`
    fn p_ident_or_fn(&mut self, scope: Ptr<Scope>) -> ParseResult<Ptr<Expr>> {
        let cur = self.bump();
        self.check_report(&TokenType::Identifier(String::new()))?;
        if self.check(&TokenType::LParenthesis) {
            self.p_fn_call(&cur, scope)
        } else {
            //* No parenthesis -> simple identifier!
            Ok(Ptr::new(Expr {
                var: ExprVariant::Ident(Identifier {
                    name: cur.get_ident().unwrap().to_owned(),
                }),
                span: cur.span,
            }))
        }
    }

    fn p_fn_call(&mut self, fn_tok: &Token, scope: Ptr<Scope>) -> ParseResult<Ptr<Expr>> {
        self.expect_report(&TokenType::LParenthesis)?;

        let func = scope
            .borrow()
            .find_def(fn_tok.get_ident().unwrap())
            .ok_or(parse_err(
                ParseErrVariant::CannotFindFn(fn_tok.get_ident().unwrap().to_owned()),
                fn_tok.span,
            ))?;

        // The expressions in function call
        let mut expr_vec = Vec::new();

        while self.expect(&TokenType::Comma) {
            expr_vec.push(self.p_base_expr(&[TokenType::Comma], scope.clone())?);
        }
        let right_span = self.cur.span;
        self.expect_report(&TokenType::RParenthesis)?;

        Ok(Ptr::new(Expr {
            var: ExprVariant::FunctionCall(FunctionCall {
                // TODO: How do we identify functions?
                func: fn_tok.get_ident().unwrap().to_owned(),
                params: expr_vec,
            }),
            span: fn_tok.span + right_span,
        }))
    }

    fn p_literal(&mut self) -> ParseResult<Ptr<Expr>> {
        let t = self.lexer.next().unwrap();
        match t.var {
            TokenType::Literal(i) => Ok(Ptr::new(Expr {
                var: ExprVariant::Literal(i.into()),
                span: t.span,
            })),
            v @ _ => Err(parse_err(
                ParseErrVariant::InternalErr(format!(
                    "Bad branching into literal parsing while getting a token type of `{}`",
                    v
                )),
                t.span,
            )),
        }
    }
}

trait IntoOperator {
    fn into_op(&self, suggest_unary: bool) -> Option<OpVar>;
}

impl TokenType {
    fn into_op(&self, unary_prefix: bool, unary_postfix: bool) -> Option<OpVar> {
        use OpVar::*;
        use TokenType::*;
        if unary_prefix {
            match self {
                Minus => Some(Neg),
                Multiply => Some(Der),
                BinaryAnd => Some(Ref),
                Increase => Some(Inb),
                Decrease => Some(Deb),
                LParenthesis => Some(_Lpr),
                _ => None,
            }
        } else if unary_postfix {
            match self {
                Increase => Some(Ina),
                Decrease => Some(Dea),
                _ => None,
            }
        } else {
            match self {
                Minus => Some(Sub),
                Plus => Some(Add),
                Multiply => Some(Mul),
                Divide => Some(Div),
                Not => Some(Inv),
                BinaryAnd => Some(Ban),
                BinaryOr => Some(Bor),
                TokenType::And => Some(OpVar::And),
                TokenType::Or => Some(OpVar::Or),
                TokenType::Xor => Some(OpVar::Xor),
                Equals => Some(Eq),
                NotEquals => Some(Neq),
                LessThan => Some(Lt),
                GreaterThan => Some(Gt),
                LessOrEqualThan => Some(Lte),
                GreaterOrEqualThan => Some(Gte),
                Assign => Some(_Asn),
                Comma => Some(_Com),
                LParenthesis => Some(_Lpr),
                RParenthesis => Some(_Rpr),
                _ => None,
            }
        }
    }
}

trait Operator {
    fn priority(&self) -> isize;
    fn is_unary(&self) -> bool;
    fn is_right_associative(&self) -> bool;
    fn is_left_associative(&self) -> bool {
        !self.is_right_associative()
    }
}

impl Operator for OpVar {
    fn priority(&self) -> isize {
        // According to https://zh.cppreference.com/w/cpp/language/operator_precedence
        use OpVar::*;
        match self {
            _Dum => 0,
            _Lpr | _Rpr => 2,
            _Com => 8,
            _Asn | _Csn => 10,
            Eq | Neq => 13,
            Gt | Lt | Gte | Lte => 14,
            Or => 15,
            And => 16,
            Bor => 17,
            Xor => 18,
            Ban => 19,
            Add | Sub => 20,
            Mul | Div => 30,
            Neg | Inv | Bin | Ref | Der | Ina | Inb | Dea | Deb => 40,
        }
    }

    fn is_unary(&self) -> bool {
        use OpVar::*;
        match self {
            Neg | Inv | Bin | Ref | Der | Ina | Inb | Dea | Deb => true,
            _ => false,
        }
    }

    fn is_right_associative(&self) -> bool {
        use OpVar::*;
        match self {
            Neg | Inv | Bin | Ref | Der | _Asn | _Lpr | _Rpr => true,
            _ => false,
        }
    }
}
