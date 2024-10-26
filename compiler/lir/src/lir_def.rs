use ty::StaticInit;

/// Defines assembly tree datatypes

#[derive(Debug, Clone, Copy)]
pub enum AssemblyType {
    Long,
    Quad,
}

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
    pub alignment: i32,
    pub init: StaticInit,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Mov {
        src: Operand,
        dest: Operand,
        ty: AssemblyType,
    },
    Movsx {
        src: Operand,
        dest: Operand,
    },
    Unary {
        op: UnaryOp,
        dest: Operand,
        ty: AssemblyType,
    },
    Binary {
        op: BinaryOp,
        src: Operand,
        dest: Operand,
        ty: AssemblyType,
    },
    Cmp(Operand, Operand, AssemblyType),
    Idiv(Operand, AssemblyType),
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
    Cdq(AssemblyType),
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
    Imm(i64),
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
    SP,
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
