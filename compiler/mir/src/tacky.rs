#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TranslationUnit {
    pub funcs: Vec<Func>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Func {
    pub name: String,
    pub params: Vec<String>,
    pub instructions: Vec<Instruction>,
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
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Val {
    Constant(i32),
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
