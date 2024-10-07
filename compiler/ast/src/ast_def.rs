use std::fmt::{Display, Formatter};
use ty::Type;

/// Defines AST datatypes

#[derive(Debug, Eq, PartialEq)]
pub struct TranslationUnit {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Decl {
    FuncDecl(FuncDecl),
    VarDecl(VarDecl),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FuncDecl {
    pub ident: String,
    pub params: Vec<String>,
    pub body: Option<Block>,
    pub storage_class: Option<StorageClass>,
    pub func_type: Type,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct VarDecl {
    pub name: String,
    pub init: Option<Expr>,
    pub storage_class: Option<StorageClass>,
    pub var_type: Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BlockItem {
    S(Stmt),
    D(Decl),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ForInit {
    Decl(VarDecl),
    Expr(Option<Expr>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Stmt {
    Compound {
        block: Block,
    },
    Return {
        expr: Expr,
    },
    Expression {
        expr: Expr,
    },
    If {
        condition: Expr,
        then: Box<Stmt>,
        otherwise: Option<Box<Stmt>>,
    },
    Goto {
        label: String,
    },
    LabeledStmt {
        label: String,
        stmt: Box<Stmt>,
    },
    Break {
        label: String,
    },
    Continue {
        label: String,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
        label: String,
    },
    DoWhile {
        body: Box<Stmt>,
        condition: Expr,
        label: String,
    },
    For {
        init: ForInit,
        condition: Option<Expr>,
        post: Option<Expr>,
        body: Box<Stmt>,
        label: String,
    },
    Switch {
        control: Expr,
        body: Box<Stmt>,
        label: String,
    },
    Case {
        constant: Expr,
        body: Box<Stmt>,
        label: String,
    },
    Default {
        body: Box<Stmt>,
        label: String,
    },
    Null,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Option<Type>,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Self {
            kind,
            ty: None,
        }
    }

    pub fn set_type(&mut self, ty: Type) {
        self.ty = Some(ty);
    }

    pub fn get_type(&self) -> Option<Type> {
        self.ty.clone()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExprKind {
    Constant(Constant),
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
    Conditional {
        condition: Box<Expr>,
        then: Box<Expr>,
        otherwise: Box<Expr>,
    },
    CompoundAssignment {
        op: BinaryOp,
        lvalue: Box<Expr>,
        expr: Box<Expr>,
    },
    PostfixInc(Box<Expr>),
    PostfixDec(Box<Expr>),
    FunctionCall {
        func: String,
        args: Vec<Expr>,
    },
    Cast {
        target_type: Type,
        expr: Box<Expr>,
    },
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Constant {
    Int(i32),
    Long(i64),
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Int(val) => {
                write!(f, "{}", val)
            }
            Constant::Long(val) => {
                write!(f, "{}", val)
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
    Inc,
    Dec,
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
