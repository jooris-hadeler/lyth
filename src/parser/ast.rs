use std::fmt::{Display, Formatter};
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

impl Display for TypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Integer(width, signed) => {
                if *signed {
                    write!(f, "i")?;
                } else {
                    write!(f, "u")?;
                }

                match width {
                    Width::Byte => write!(f, "8"),
                    Width::Word => write!(f, "16"),
                    Width::DoubleWord => write!(f, "32"),
                    Width::QuadWord => write!(f, "64"),
                }
            }
            TypeKind::Boolean => write!(f, "bool"),
            TypeKind::Identifier(name) => write!(f, "{name}"),
            TypeKind::Ref(typ) => write!(f, "&{typ}"),
            TypeKind::Function(params, return_type) => {
                write!(f, "fn(")?;

                let mut first = true;
                for param in params {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{param}")?;
                }

                write!(f, ")")?;

                if let Some(return_type) = return_type {
                    write!(f, " -> {return_type}")?;
                }

                Ok(())
            }
            TypeKind::Void => write!(f, "void")
        }
    }
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
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
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