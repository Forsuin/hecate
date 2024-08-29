/// Defines AST datatypes

#[derive(Debug, Eq, PartialEq)]
pub struct TranslationUnit {
    pub func: Func,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Func {
    pub ident: String,
    pub body: Vec<BlockItem>
}

#[derive(Debug, Eq, PartialEq)]
pub enum BlockItem {
    S(Stmt),
    D(Decl),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Decl {
    pub name: String,
    pub init: Option<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Stmt {
    Return { expr: Expr },
    Expression { expr: Expr },
    Null,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expr {
    Constant(i32),
    Var(String),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Assignment {
        lvalue: Box<Expr>,
        expr: Box<Expr>,
    },
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
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
