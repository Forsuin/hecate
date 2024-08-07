/// Defines AST datatypes

#[derive(Debug, Eq, PartialEq)]
pub struct TranslationUnit {
    pub func: Func,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Func {
    pub ident: String,
    pub body: Stmt,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Stmt {
    Return { expr: Expr },
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Constant(i32),
    Unary { op: UnaryOp, expr: Box<Expr> },
    Binary { op: BinaryOp, left: Box<Expr>, right: Box<Expr> },
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    // Logical and Relational Operators
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Bitwise Operators
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitshiftLeft,
    BitshiftRight,
}