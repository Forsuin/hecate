/// Defines assembly tree datatypes

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Func(Func),
    StaticVar(StaticVar),
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub global: bool,
}

#[derive(Debug, Clone)]
pub struct StaticVar {
    pub name: String,
    pub global: bool,
    pub init: i32,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Mov {
        src: Operand,
        dest: Operand,
    },
    Unary {
        op: UnaryOp,
        dest: Operand,
    },
    Binary {
        op: BinaryOp,
        src: Operand,
        dest: Operand,
    },
    Cmp(Operand, Operand),
    Idiv(Operand),
    Jmp {
        label: String,
    },
    JmpCond {
        condition: Condition,
        label: String,
    },
    SetCond {
        condition: Condition,
        dest: Operand,
    },
    Label(String),
    Cdq,
    AllocateStack(i32),
    DeallocateStack(i32),
    Push(Operand),
    Call(String),
    Ret,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    And,
    Or,
    Xor,
    Sal,
    Sar,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Imm(i32),
    Register(Register),
    Pseudo(String),
    Stack(i32),
    Data(String),
}

#[derive(Debug, Clone)]
pub enum Register {
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
}

#[derive(Debug, Clone)]
pub enum Condition {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}
