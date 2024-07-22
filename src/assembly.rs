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
    Ret,
}

#[derive(Debug)]
pub enum Operand {
    Imm(i32),
    Register,
}
