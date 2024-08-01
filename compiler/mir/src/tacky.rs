#[derive(Debug, Eq, PartialEq)]
pub struct TranslationUnit {
    pub func: Func,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Func {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Return(Val),
    Unary { op: UnaryOp, src: Val, dest: Val },
    Binary { op: BinaryOp, first: Val, second: Val, dest: Val },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Val {
    Constant(i32),
    Var(String),
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negate,
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}