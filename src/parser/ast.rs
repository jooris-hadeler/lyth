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
    pub kind: TypeKind,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Integer(Width, bool),
    Boolean,

    Ref(Box<TypeKind>),
    Identifier(Box<str>),
    Function(Vec<TypeKind>, Option<Box<TypeKind>>),

    Void,
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Ord, Eq, Copy)]
pub enum Width {
    Byte,
    Word,
    DoubleWord,
    QuadWord,
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
    Let(Box<str>, Type, Option<Expr>),
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
    Integer(usize, bool),
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

pub trait ASTVisitor {
    type Error;

    type TypeDefReturn;
    fn visit_type_def(&mut self, type_def: &TypeDef) -> Result<Self::TypeDefReturn, Self::Error>;

    type FuncDefReturn;
    fn visit_func_def(&mut self, func_def: &FuncDef) -> Result<Self::FuncDefReturn, Self::Error>;

    type BlockReturn;
    fn visit_block(&mut self, block: &Block) -> Result<Self::BlockReturn, Self::Error>;

    type StmtReturn;
    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<Self::StmtReturn, Self::Error>;

    type ExprReturn;
    fn visit_expr(&mut self, expr: &Expr) -> Result<Self::ExprReturn, Self::Error>;
}