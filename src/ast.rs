/// Representation of a Module
#[derive(Debug, PartialEq)]
pub struct Module {
    /// Name of the module
    pub name: String,
    /// File of the module
    pub file: String,
    /// Functions in the module
    pub funcs: Vec<Function>,
    /// Types in the module
    pub types: Vec<Struct>,
}

/// Representation of a Function
#[derive(Debug, PartialEq)]
pub struct Function {
    /// Name of the function
    pub name: String,
    /// Parameters to the function
    pub params: Option<Vec<Parameter>>,
    /// Return type of the function
    pub rtyp: Option<Type>,
    /// Body of the function
    pub body: Box<Stmt>,
}

/// Representation of a Struct
#[derive(Debug, PartialEq)]
pub struct Struct {
    /// Name of the struct
    pub name: String,
    /// Fields of the struct
    pub fields: Vec<Field>,
}

/// Representation of a Field
type Field = (String, Type);

/// Representation of a Parameter
type Parameter = (String, Type);

/// Representation of a Statement
#[derive(Debug, PartialEq)]
pub enum Stmt {
    /// Represents an Expression Statement like test();
    Expression(Box<Expr>),
    /// Represents a Let Statement
    Let(String, Option<Type>, Box<Expr>),
    /// Represents an If Statement
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    /// Represents a While Statement
    While(Box<Expr>, Box<Stmt>),
    /// Represents a Block Statement
    Block(Vec<Box<Stmt>>),
    /// Represents a Return Statement
    Return(Option<Box<Expr>>),
}

/// Representation of a Type
#[derive(Debug, PartialEq)]
pub enum Type {
    U8,
    U16,
    U32,
    U64,
    S8,
    S16,
    S32,
    S64,
    Boolean,

    Custom(String),
    Pointer(Box<Type>),
}

/// Representation of an Expression
#[derive(Debug, PartialEq)]
pub enum Expr {
    /// Represents a Unary Expression, like -1, !1 or ~1
    Unary(Operation, Box<Expr>),
    /// Represents a Binary Expression, like 1 + 1 or 1 - 1
    Binary(Operation, Box<Expr>, Box<Expr>),
    /// Represents a Literal Expression, like 1, "abc" or true
    Literal(Lit),
    /// Represents a Member Expression
    Member(Box<Expr>, String),
    /// Represents a Call Expression
    Call(Box<Expr>, Option<Vec<Box<Expr>>>),
    /// Represents a Identifier Expression
    Identifier(String),
}

/// Representation of a Literal
#[derive(Debug, PartialEq)]
pub enum Lit {
    /// Represents an Integer Literal
    Integer(usize),
    /// Represents a String Literal
    String(String),
    /// Represents a Boolean Literal
    Boolean(bool),
    /// Represents a Struct Literal
    Struct(String, Vec<(String, Box<Expr>)>),
}

#[derive(Debug, PartialEq)]
pub enum Operation {
    // Mathematical
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Unary
    Neg,
    Not,
    AddrOf,
    Deref,
    LogicalNot,

    // Bitwise
    And,
    Or,
    Xor,
    Shl,
    Shr,

    // Logical
    LogicalAnd,
    LogicalOr,

    // Relational and Equality
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,

    // Assign and Compound Assign
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    ShlAssign,
    ShrAssign,
    AndAssign,
    OrAssign,
    XorAssign,
}
