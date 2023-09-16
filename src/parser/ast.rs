use crate::lexer::Location;

#[derive(Debug, Clone)]
pub struct Module {
    pub file: Box<str>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub name: Box<str>,
    pub loc: Location
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Box<str>,
    pub typ: Type,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Box<str>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Option<Block>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Stmt>);

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Let(Box<str>, Option<Type>, Option<Expr>),
    Return(Option<Expr>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Integer(usize),
    Identifier(Box<str>),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>)
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,

    Equals,
    Unequals,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,

    BinaryAnd,
    BinaryOr,
    BinaryXor,

    LogicalAnd,
    LogicalOr,
}