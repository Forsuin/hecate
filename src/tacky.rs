#[derive(Debug)]
pub struct TranslationUnit {
    pub func: Func,
}

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Return(Val),
    Unary { op: UnaryOp, src: Val, dest: Val },
}

#[derive(Debug, Clone)]
pub enum Val {
    Constant(i32),
    Var(String),
}

#[derive(Debug)]
pub enum UnaryOp {
    Complement,
    Negate,
}
