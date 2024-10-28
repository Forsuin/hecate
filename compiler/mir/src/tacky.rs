use ty::{Constant, StaticInit, Type};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TranslationUnit {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Decl {
    Func(Func),
    StaticVar(StaticVar),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StaticVar {
    pub name: String,
    pub global: bool,
    pub ty: Type,
    pub init: StaticInit,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Func {
    pub name: String,
    pub params: Vec<String>,
    pub instructions: Vec<Instruction>,
    pub global: bool,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Instruction {
    Return(Val),
    Unary {
        op: UnaryOp,
        src: Val,
        dest: Val,
    },
    Binary {
        op: BinaryOp,
        first: Val,
        second: Val,
        dest: Val,
    },
    Copy {
        src: Val,
        dest: Val,
    },
    Jump {
        target: String,
    },
    JumpIfZero {
        condition: Val,
        target: String,
    },
    JumpIfNotZero {
        condition: Val,
        target: String,
    },
    FunCall {
        func_name: String,
        args: Vec<Val>,
        dest: Val,
    },
    Label(String),
    SignExtend {
        src: Val,
        dest: Val,
    },
    ZeroExtend {
        src: Val,
        dest: Val,
    },
    Truncate {
        src: Val,
        dest: Val,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Val {
    Constant(Constant),
    Var(String),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    // Logical
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Bitwise
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitshiftLeft,
    BitshiftRight,
}
