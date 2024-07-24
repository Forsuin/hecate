/// Defines assembly tree datatypes

#[derive(Debug)]
pub struct Program {
    pub func: Function,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Mov { src: Operand, dest: Operand },
    Unary{ op: UnaryOp, dest: Operand },
    AllocateStack(i32),
    Ret,
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug)]
pub enum Operand {
    Imm(i32),
    Register(Register),
    Pseudo(String),
    Stack(i32),
}

#[derive(Debug)]
pub enum Register {
    AX,
    R10,
}
