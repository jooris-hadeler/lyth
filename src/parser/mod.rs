use crate::lexer::{Location, Token, TokenKind};
use crate::parser::ast::{BinaryOp, ExprKind, Param, StmtKind, StructField, TypeKind, UnaryOp, Width};

use thiserror::Error;

pub mod ast;

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

    Assign,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equality,
    Order,
    Shift,
    Sum,
    Product,

    Call,
    Index,
    Access,

    Highest,
}

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    file: Box<str>,
}

impl Parser {
    /// This function constructs a new Parser from a vector of Tokens and the file path.
    pub fn new(tokens: Vec<Token>, file: Box<str>) -> Self {
        Self { tokens, index: 0, file }
    }

    /// This function `consumes` a Token,
    /// this means it returns the token and increments the index.
    fn consume(&mut self) -> Result<Token, ParserError> {
        let token = self.tokens.get(self.index)
            .ok_or(ParserError::Message("Unexpected end of file.".into()))?.clone();

        self.index += 1;

        Ok(token)
    }

    /// This function returns a copy of a Token at index + offset.
    fn peek(&self, offset: usize) -> Option<Token> {
        self.tokens.get(self.index + offset).cloned()
    }

    /// This function allows checks on a peek Token using a predicate.
    fn is_peek_predicate(&self, offset: usize, predicate: impl FnOnce(Token) -> bool) -> bool {
        self.peek(offset).is_some_and(predicate)
    }

    /// This function allows checks if a Token is of certain kind.
    fn is_peek(&self, offset: usize, kind: TokenKind) -> bool {
        self.is_peek_predicate(offset, |tok| tok.kind == kind)
    }

    /// This function tries to consume a Token if it
    /// matches a predicate, otherwise it returns an Error.
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

    /// This function tries to consume a Token if it
    /// matches a predicate otherwise it returns None.
    ///
    /// WARNING: consuming the Token can still return an error.
    fn try_consume_predicate(&mut self, predicate: impl FnOnce(Token) -> bool) -> Result<Option<Token>, ParserError> {
        if self.peek(0).is_some_and(predicate) {
            Ok(Some(self.consume()?))
        } else {
            Ok(None)
        }
    }

    /// This function tries to consume a Token of certain kind,
    /// otherwise it returns an error.
    fn try_consume_with_error<S: Into<Box<str>>>(&mut self, kind: TokenKind, err: S) -> Result<Token, ParserError> {
        self.try_consume_predicate_with_error(|tok| tok.kind == kind, err)
    }

    /// This function tries to consume an error of certain kind,
    /// otherwise it returns None.
    fn try_consume(&mut self, kind: TokenKind) -> Result<Option<Token>, ParserError> {
        self.try_consume_predicate(|tok| tok.kind == kind)
    }

    /// This function tries to consume an identifier, it returns
    /// the literal of the identifier and the location.
    fn try_consume_identifier<S: Into<Box<str>>>(&mut self, msg: S) -> Result<(Box<str>, Location), ParserError> {
        let token = self.try_consume_predicate_with_error(
            |tok| matches!(tok.kind, TokenKind::Identifier(..)),
            msg.into(),
        )?;

        let name = token.kind.unwrap_identifier();
        let loc = token.loc.clone();

        Ok((name, loc))
    }

    /// This function parses a module.
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

    /// This function parses a StructDef.
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

    /// This function parses the struct body.
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

    /// This function parses a struct field.
    pub fn parse_struct_field(&mut self) -> Result<StructField, ParserError> {
        let (name, loc) = self.try_consume_identifier("expected identifier after `struct` keyword")?;

        self.try_consume_with_error(TokenKind::Colon, "expected `:` after field name")?;

        let typ = self.parse_type()?;

        Ok((name, loc, typ))
    }

    /// This function parses a FuncDef.
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

    /// This function parses function parameters.
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

    /// This function parses a type.
    pub fn parse_type(&mut self) -> Result<ast::Type, ParserError> {
        if self.is_peek(0, TokenKind::Ampersand) {
            self.consume()?;
            let (kind, loc) = self.parse_type_kind()?;

            Ok(ast::Type { kind: TypeKind::Ref(Box::new(kind)), loc })
        } else {
            let (kind, loc) = self.parse_type_kind()?;

            Ok(ast::Type { kind, loc })
        }
    }

    pub fn parse_type_kind(&mut self) -> Result<(ast::TypeKind, Location), ParserError> {
        match self.peek(0) {
            Some(tok) => {
                let kind = match tok.kind {
                    TokenKind::I8 => TypeKind::Integer(Width::Byte, true),
                    TokenKind::I16 => TypeKind::Integer(Width::Word, true),
                    TokenKind::I32 => TypeKind::Integer(Width::DoubleWord, true),
                    TokenKind::I64 => TypeKind::Integer(Width::QuadWord, true),

                    TokenKind::U8 => TypeKind::Integer(Width::Byte, false),
                    TokenKind::U16 => TypeKind::Integer(Width::Word, false),
                    TokenKind::U32 => TypeKind::Integer(Width::DoubleWord, false),
                    TokenKind::U64 => TypeKind::Integer(Width::QuadWord, false),

                    TokenKind::Bool => TypeKind::Boolean,
                    TokenKind::Identifier(name) => TypeKind::Identifier(name.clone()),

                    _ => return Err(ParserError::LocatedError {
                        msg: "expected type".into(),
                        loc: tok.loc.clone(),
                    })
                };

                self.consume()?;

                Ok((kind, tok.loc.clone()))
            }
            None => Err(ParserError::Message("unexpected end of file".into()))
        }
    }

    /// This function parses a block.
    pub fn parse_block(&mut self) -> Result<ast::Block, ParserError> {
        let mut stmts = Vec::new();

        self.try_consume_with_error(TokenKind::LBrace, "expected `{` after function signature")?;

        while !self.is_peek(0, TokenKind::RBrace) {
            stmts.push(self.parse_stmt()?);
        }

        self.try_consume_with_error(TokenKind::RBrace, "expected `}` after end of block")?;

        Ok(ast::Block(stmts))
    }

    /// This function parses a statement.
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

    /// This function parses an expression statement.
    pub fn parse_expr_stmt(&mut self) -> Result<ast::Stmt, ParserError> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        let loc = expr.loc.clone();

        self.try_consume_with_error(TokenKind::SemiColon, "expected `;` after an expression")?;

        Ok(ast::Stmt {
            kind: StmtKind::Expr(expr),
            loc,
        })
    }

    /// This function parses a let statement.
    pub fn parse_let_stmt(&mut self) -> Result<ast::Stmt, ParserError> {
        self.try_consume_with_error(TokenKind::Let, "expected `let` keyword")?;

        let (name, loc) = self.try_consume_identifier("expected identifier after `fn` keyword")?;

        self.try_consume(TokenKind::Colon)?;
        let typ = self.parse_type()?;

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

    /// This function parses a return statement.
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

    /// This function returns the precedence of the Token at position index + offset.
    fn peek_precedence(&self, offset: usize) -> Precedence {
        match self.peek(offset) {
            Some(token) => match token.kind {
                TokenKind::Plus
                | TokenKind::Minus => Precedence::Sum,

                TokenKind::Asterisk
                | TokenKind::Slash => Precedence::Product,

                TokenKind::Equal
                | TokenKind::Unequal => Precedence::Equality,

                TokenKind::LessThan
                | TokenKind::LessEqual
                | TokenKind::GreaterThan
                | TokenKind::GreaterEqual => Precedence::Order,

                TokenKind::ShiftLeft
                | TokenKind::ShiftRight => Precedence::Shift,

                TokenKind::Ampersand => Precedence::BitwiseAnd,
                TokenKind::Pipe => Precedence::BitwiseOr,
                TokenKind::Caret => Precedence::BitwiseXor,
                TokenKind::DAmpersand => Precedence::LogicalAnd,
                TokenKind::DPipe => Precedence::LogicalOr,
                TokenKind::Assign => Precedence::Assign,
                TokenKind::LParen => Precedence::Call,
                TokenKind::Dot => Precedence::Access,

                _ => Precedence::Highest,
            }
            None => Precedence::Lowest
        }
    }

    /// This function parses an expression.
    ///
    /// param `precedence`: The precedence of the current expression.
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
                    | TokenKind::Unequal
                    | TokenKind::Ampersand
                    | TokenKind::Pipe
                    | TokenKind::Caret
                    | TokenKind::DAmpersand
                    | TokenKind::DPipe
                    | TokenKind::ShiftLeft
                    | TokenKind::ShiftRight
                    | TokenKind::LessThan
                    | TokenKind::LessEqual
                    | TokenKind::GreaterThan
                    | TokenKind::GreaterEqual
                    | TokenKind::Assign
                    | TokenKind::Dot => self.parse_infix_expression(left)?,

                    TokenKind::LParen => self.parse_call_expression(left)?,

                    _ => return Ok(left),
                }
                None => return Err(ParserError::Message("unexpected end of file".into()))
            };
        }

        Ok(left)
    }

    /// This function parses a call expression.
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

    /// This function parses call parameters.
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

    /// This function parses a boolean literal.
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

    /// This function parses an integer literal.
    pub fn parse_integer_literal(&mut self) -> Result<ast::Expr, ParserError> {
        let tok = self.try_consume_predicate_with_error(
            |tok| matches!(tok.kind, TokenKind::Integer(..)),
            "expected integer literal",
        )?;

        let (value, unsigned) = match tok.kind {
            TokenKind::Integer(value, signed) => (value, signed),
            _ => unreachable!(),
        };

        Ok(ast::Expr {
            kind: ExprKind::Integer(value, unsigned),
            loc: tok.loc.clone(),
        })
    }

    /// This function parses an identifier literal in an expression.
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

    /// This function parses a prefix expression.
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

    /// This function parses an infix expression.
    ///
    /// param `left`: the left side of the infix expression
    pub fn parse_infix_expression(&mut self, left: ast::Expr) -> Result<ast::Expr, ParserError> {
        let op = match self.peek(0) {
            Some(token) => match token.kind {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Subtract,
                TokenKind::Asterisk => BinaryOp::Multiply,
                TokenKind::Slash => BinaryOp::Divide,
                TokenKind::Equal => BinaryOp::Equals,
                TokenKind::Unequal => BinaryOp::Unequals,
                TokenKind::Ampersand => BinaryOp::BinaryAnd,
                TokenKind::Pipe => BinaryOp::BinaryOr,
                TokenKind::Caret => BinaryOp::BinaryXor,
                TokenKind::DAmpersand => BinaryOp::LogicalAnd,
                TokenKind::DPipe => BinaryOp::LogicalOr,
                TokenKind::ShiftLeft => BinaryOp::ShiftLeft,
                TokenKind::ShiftRight => BinaryOp::ShiftRight,
                TokenKind::LessThan => BinaryOp::LessThan,
                TokenKind::LessEqual => BinaryOp::LessEqual,
                TokenKind::GreaterThan => BinaryOp::GreaterThan,
                TokenKind::GreaterEqual => BinaryOp::GreaterEqual,
                TokenKind::Assign => BinaryOp::Assign,
                TokenKind::Dot => BinaryOp::Access,

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

