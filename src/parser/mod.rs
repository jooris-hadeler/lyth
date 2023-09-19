use crate::lexer::{Location, Token, TokenKind};
use crate::parser::ast::{BinaryOp, ExprKind, Param, StmtKind, StructField, UnaryOp};

use thiserror::Error;

mod ast;

#[derive(Clone, Debug, Error)]
pub enum ParserError {
    #[error("{0}")]
    Message(Box<str>),

    #[error("{0}:{1}-{2}: {3}", loc.file, loc.start, loc.end, msg)]
    LocatedError {
        loc: Location,
        msg: Box<str>,
    },
}

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
pub enum Precedence {
    Lowest,

    Equality,
    Sum,
    Product,

    Call,
    Index,

    Highest,
}

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    file: Box<str>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, file: Box<str>) -> Self {
        Self { tokens, index: 0, file }
    }

    fn consume(&mut self) -> Result<Token, ParserError> {
        let token = self.tokens.get(self.index)
            .ok_or(ParserError::Message("Unexpected end of file.".into()))?.clone();

        self.index += 1;

        Ok(token)
    }

    fn peek(&self, offset: usize) -> Option<Token> {
        self.tokens.get(self.index + offset).cloned()
    }

    fn is_peek_predicate(&self, offset: usize, predicate: impl FnOnce(Token) -> bool) -> bool {
        self.peek(offset).is_some_and(predicate)
    }

    fn is_peek(&self, offset: usize, kind: TokenKind) -> bool {
        self.is_peek_predicate(offset, |tok| tok.kind == kind)
    }

    fn try_consume_predicate_with_error<S: Into<Box<str>>>(&mut self, predicate: impl FnOnce(Token) -> bool, msg: S) -> Result<Token, ParserError> {
        if self.peek(0).is_some_and(predicate) {
            self.consume()
        } else {
            let tok = self.peek(0)
                .ok_or(ParserError::Message("Unexpected end of file.".into()))?;

            Err(ParserError::LocatedError {
                loc: tok.loc.clone(),
                msg: msg.into(),
            })
        }
    }

    fn try_consume_predicate(&mut self, predicate: impl FnOnce(Token) -> bool) -> Result<Option<Token>, ParserError> {
        if self.peek(0).is_some_and(predicate) {
            Ok(Some(self.consume()?))
        } else {
            Ok(None)
        }
    }

    fn try_consume_with_error<S: Into<Box<str>>>(&mut self, kind: TokenKind, err: S) -> Result<Token, ParserError> {
        self.try_consume_predicate_with_error(|tok| tok.kind == kind, err)
    }

    fn try_consume(&mut self, kind: TokenKind) -> Result<Option<Token>, ParserError> {
        self.try_consume_predicate(|tok| tok.kind == kind)
    }

    fn try_consume_identifier<S: Into<Box<str>>>(&mut self, msg: S) -> Result<(Box<str>, Location), ParserError> {
        let token = self.try_consume_predicate_with_error(
            |tok| matches!(tok.kind, TokenKind::Identifier(..)),
            msg.into()
        )?;

        let name = token.kind.unwrap_identifier();
        let loc = token.loc.clone();

        Ok((name, loc))
    }

    pub fn parse_module(&mut self) -> Result<ast::Module, ParserError> {
        let mut functions = Vec::new();
        let mut types = Vec::new();

        while !self.is_peek(0, TokenKind::Eof) {
            match self.peek(0) {
                Some(tok) => match tok.kind {
                    TokenKind::Fn => functions.push(self.parse_func_def()?),
                    TokenKind::Struct => types.push(self.parse_struct_def()?),
                    _ => unimplemented!("unimplemented token kind: {:?}", tok.kind)
                },
                None => break,
            }
        }

        Ok(ast::Module {
            file: self.file.clone(),
            functions,
            types,
        })
    }

    pub fn parse_struct_def(&mut self) -> Result<ast::TypeDef, ParserError> {
        self.try_consume_with_error(TokenKind::Struct, "expected keyword `struct`")?;

        let (name, loc) = self.try_consume_identifier("expected identifier after `struct` keyword")?;

        let fields = self.parse_struct_body()?;

        Ok(ast::TypeDef::Struct(ast::TypeDefStruct {
            name,
            fields,
            loc,
        }))
    }

    pub fn parse_struct_body(&mut self) -> Result<Vec<StructField>, ParserError> {
        let mut fields = Vec::new();

        self.try_consume_with_error(TokenKind::LBrace, "expected `{` at beginning of struct body")?;

        if self.is_peek_predicate(0, |tok| matches!(tok.kind, TokenKind::Identifier(..))) {
            loop {
                fields.push(self.parse_struct_field()?);

                if self.is_peek(0, TokenKind::Comma) {
                    self.consume()?;
                } else { break; }
            }
        }

        self.try_consume_with_error(TokenKind::RBrace, "expected `}` at end of struct body")?;

        Ok(fields)
    }

    pub fn parse_struct_field(&mut self) -> Result<StructField, ParserError> {
        let (name, loc) = self.try_consume_identifier("expected identifier after `struct` keyword")?;

        self.try_consume_with_error(TokenKind::Colon, "expected `:` after field name")?;

        let typ = self.parse_type()?;

        Ok((name, loc, typ))
    }

    pub fn parse_func_def(&mut self) -> Result<ast::FuncDef, ParserError> {
        self.try_consume_with_error(TokenKind::Fn, "expected keyword `fn`")?;

        let (name, loc) = self.try_consume_identifier("expected identifier after `fn` keyword")?;

        self.try_consume_with_error(TokenKind::LParen, "expected `(` after function name")?;

        let params = self.parse_function_params()?;

        self.try_consume_with_error(TokenKind::RParen, "expected `)` after parameter declarations")?;

        let return_type = if self.is_peek(0, TokenKind::Arrow) {
            self.consume()?;
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = if self.is_peek(0, TokenKind::SemiColon) {
            self.consume()?;
            None
        } else {
            Some(self.parse_block()?)
        };

        Ok(ast::FuncDef {
            name,
            params,
            return_type,
            body,
            loc,
        })
    }

    pub fn parse_function_params(&mut self) -> Result<Vec<ast::Param>, ParserError> {
        let mut params = Vec::new();

        while !self.is_peek(0, TokenKind::RParen) {
            let (name, loc) = self.try_consume_identifier("expected identifier as parameter name")?;

            self.try_consume_with_error(TokenKind::Colon, "expected `:` after parameter name")?;

            let typ = self.parse_type()?;

            params.push(Param { name, loc, typ });

            if self.is_peek(0, TokenKind::Comma) {
                self.consume()?;
            }
        }

        Ok(params)
    }

    pub fn parse_type(&mut self) -> Result<ast::Type, ParserError> {
        let (name, loc) = self.try_consume_identifier("expected identifier as type")?;

        Ok(ast::Type { name, loc })
    }

    pub fn parse_block(&mut self) -> Result<ast::Block, ParserError> {
        let mut stmts = Vec::new();

        self.try_consume_with_error(TokenKind::LBrace, "expected `{` after function signature")?;

        while !self.is_peek(0, TokenKind::RBrace) {
            stmts.push(self.parse_stmt()?);
        }

        self.try_consume_with_error(TokenKind::RBrace, "expected `}` after end of block")?;

        Ok(ast::Block(stmts))
    }

    pub fn parse_stmt(&mut self) -> Result<ast::Stmt, ParserError> {
        match self.peek(0) {
            Some(token) => match token.kind {
                TokenKind::Let => self.parse_let_stmt(),
                TokenKind::Return => self.parse_return_stmt(),

                _ => self.parse_expr_stmt(),
            }
            None => return Err(ParserError::Message("unexpected end of file".into())),
        }
    }

    pub fn parse_expr_stmt(&mut self) -> Result<ast::Stmt, ParserError> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        let loc = expr.loc.clone();

        self.try_consume_with_error(TokenKind::SemiColon, "expected `;` after an expression")?;

        Ok(ast::Stmt {
            kind: StmtKind::Expr(expr),
            loc,
        })
    }

    pub fn parse_let_stmt(&mut self) -> Result<ast::Stmt, ParserError> {
        self.try_consume_with_error(TokenKind::Let, "expected `let` keyword")?;

        let (name, loc) = self.try_consume_identifier("expected identifier after `fn` keyword")?;

        let typ = if self.is_peek(0, TokenKind::Colon) {
            self.consume()?;
            Some(self.parse_type()?)
        } else {
            None
        };

        let expr = if self.is_peek(0, TokenKind::SemiColon) {
            self.consume()?;
            None
        } else {
            self.try_consume_with_error(TokenKind::Assign, "expected `=` after let signature")?;

            let value = self.parse_expression(Precedence::Lowest)?;

            self.try_consume_with_error(TokenKind::SemiColon, "expected `;` at the end of a let stmt")?;

            Some(value)
        };

        Ok(ast::Stmt {
            kind: StmtKind::Let(name, typ, expr),
            loc,
        })
    }

    pub fn parse_return_stmt(&mut self) -> Result<ast::Stmt, ParserError> {
        let tok = self.try_consume_with_error(TokenKind::Return, "expected `return`")?;

        let value = if self.is_peek(0, TokenKind::SemiColon) {
            self.consume()?;
            None
        } else {
            let value = self.parse_expression(Precedence::Lowest)?;

            self.try_consume_with_error(TokenKind::SemiColon, "expected `;` at the end of a return stmt")?;

            Some(value)
        };

        Ok(ast::Stmt {
            kind: StmtKind::Return(value),
            loc: tok.loc.clone(),
        })
    }

    fn peek_precedence(&self, offset: usize) -> Precedence {
        match self.peek(offset) {
            Some(token) => match token.kind {
                TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
                TokenKind::Asterisk | TokenKind::Slash => Precedence::Product,
                TokenKind::Equal | TokenKind::Unequal => Precedence::Equality,
                TokenKind::LParen => Precedence::Call,

                _ => Precedence::Highest,
            }
            None => Precedence::Lowest
        }
    }

    pub fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expr, ParserError> {
        // this parses all prefix operations like -x or !x
        let mut left = match self.peek(0) {
            Some(token) => match token.kind {
                TokenKind::Integer(..) => self.parse_integer_literal()?,
                TokenKind::True | TokenKind::False => self.parse_boolean_literal()?,
                TokenKind::Identifier(..) => self.parse_identifier()?,

                TokenKind::Minus => self.parse_prefix_expression()?,
                TokenKind::Bang => self.parse_prefix_expression()?,

                _ => return Err(ParserError::LocatedError { loc: token.loc.clone(), msg: "expected integer, `-` or `!`".into() }),
            }
            None => return Err(ParserError::Message("unexpected end of file".into()))
        };

        // this parses all infix expressions like a + b or a == b
        while !self.is_peek(0, TokenKind::SemiColon)
            && precedence < self.peek_precedence(0) {
            left = match self.peek(0) {
                Some(token) => match token.kind {
                    TokenKind::Plus
                    | TokenKind::Minus
                    | TokenKind::Asterisk
                    | TokenKind::Slash
                    | TokenKind::Equal
                    | TokenKind::Unequal => self.parse_infix_expression(left)?,
                    TokenKind::LParen => self.parse_call_expression(left)?,

                    _ => return Ok(left),
                }
                None => return Err(ParserError::Message("unexpected end of file".into()))
            };
        }

        Ok(left)
    }

    pub fn parse_call_expression(&mut self, func: ast::Expr) -> Result<ast::Expr, ParserError> {
        self.try_consume_with_error(TokenKind::LParen, "expected `(` after function")?;

        let params = self.parse_call_params()?;

        self.try_consume_with_error(TokenKind::RParen, "expected `)` after call params")?;

        let loc = func.loc.clone();

        Ok(ast::Expr {
            kind: ExprKind::Call(Box::new(func), params),
            loc,
        })
    }

    pub fn parse_call_params(&mut self) -> Result<Vec<ast::Expr>, ParserError> {
        let mut params = Vec::new();

        if self.is_peek(0, TokenKind::RParen) {
            return Ok(params);
        }

        loop {
            params.push(self.parse_expression(Precedence::Lowest)?);

            if self.is_peek(0, TokenKind::Comma) { self.consume()?; } else { break; }
        }

        Ok(params)
    }

    pub fn parse_boolean_literal(&mut self) -> Result<ast::Expr, ParserError> {
        let tok = self.try_consume_predicate_with_error(
            |tok| matches!(tok.kind, TokenKind::True | TokenKind::False),
            "expected `true` or `false`",
        )?;

        Ok(ast::Expr {
            kind: ExprKind::Boolean(tok.kind == TokenKind::True),
            loc: tok.loc.clone(),
        })
    }

    pub fn parse_integer_literal(&mut self) -> Result<ast::Expr, ParserError> {
        let tok = self.try_consume_predicate_with_error(
            |tok| matches!(tok.kind, TokenKind::Integer(..)),
            "expected integer literal",
        )?;

        let value = match tok.kind {
            TokenKind::Integer(value) => value,
            _ => unreachable!(),
        };

        Ok(ast::Expr {
            kind: ExprKind::Integer(value),
            loc: tok.loc.clone(),
        })
    }

    pub fn parse_identifier(&mut self) -> Result<ast::Expr, ParserError> {
        let ident = self.try_consume_predicate_with_error(
            |tok| matches!(tok.kind, TokenKind::Identifier(..)),
            "expected identifier",
        )?;

        let name = match ident.kind {
            TokenKind::Identifier(name) => name,
            _ => unreachable!()
        };

        Ok(ast::Expr {
            kind: ExprKind::Identifier(name),
            loc: ident.loc.clone(),
        })
    }

    pub fn parse_prefix_expression(&mut self) -> Result<ast::Expr, ParserError> {
        let (mut loc, op) = match self.peek(0) {
            Some(token) => (token.loc.clone(), match token.kind {
                TokenKind::Minus => UnaryOp::Negate,
                TokenKind::Bang => UnaryOp::Not,

                // this should not happen
                // this is most likely a result of this function
                // being called from another function than parse_expression
                _ => unreachable!()
            }),
            None => unreachable!()
        };

        self.consume()?;

        let right = self.parse_expression(Precedence::Lowest)?;
        loc.end = right.loc.end;

        Ok(ast::Expr {
            kind: ExprKind::Unary(op, Box::new(right)),
            loc,
        })
    }

    pub fn parse_infix_expression(&mut self, left: ast::Expr) -> Result<ast::Expr, ParserError> {
        let op = match self.peek(0) {
            Some(token) => match token.kind {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Subtract,
                TokenKind::Asterisk => BinaryOp::Multiply,
                TokenKind::Slash => BinaryOp::Divide,
                TokenKind::Equal => BinaryOp::Equals,
                TokenKind::Unequal => BinaryOp::Unequals,

                // this should not happen
                // this is most likely a result of this function
                // being called from another function than parse_expression
                _ => unreachable!()
            }
            None => return Err(ParserError::Message("unexpected end of file".into())),
        };

        let precedence = self.peek_precedence(0);
        self.consume()?;

        let right = self.parse_expression(precedence)?;

        let loc = Location {
            file: self.file.clone(),
            start: left.loc.start,
            end: right.loc.end,
        };

        Ok(ast::Expr {
            kind: ExprKind::Binary(op, Box::new(left), Box::new(right)),
            loc,
        })
    }
}

