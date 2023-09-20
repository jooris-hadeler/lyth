use crate::lexer::Location;

#[derive(Debug, Clone)]
pub struct Module {
    pub file: Box<str>,
    pub functions: Vec<FuncDef>,
    pub types: Vec<TypeDef>,
}

#[derive(Debug, Clone)]
pub enum TypeDef {
    Struct(TypeDefStruct)
}

pub type StructField = (Box<str>, Location, Type);

#[derive(Debug, Clone)]
pub struct TypeDefStruct {
    pub name: Box<str>,
    pub fields: Vec<StructField>,
    pub loc: Location,
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
pub struct FuncDef {
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
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Call(Box<Expr>, Vec<Expr>),
    Integer(usize),
    Boolean(bool),
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
    Assign,
    Access,

    Add,
    Subtract,
    Multiply,
    Divide,

    Equals,
    Unequals,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,

    BinaryAnd,
    BinaryOr,
    BinaryXor,

    LogicalAnd,
    LogicalOr,

    ShiftLeft,
    ShiftRight,
}