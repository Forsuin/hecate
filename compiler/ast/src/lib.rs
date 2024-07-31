/// Defines AST datatypes

#[derive(Debug)]
pub struct TranslationUnit {
    pub func: Func,
}

#[derive(Debug)]
pub struct Func {
    pub ident: String,
    pub body: Stmt,
}

#[derive(Debug)]
pub enum Stmt {
    Return { expr: Expr },
}

#[derive(Debug)]
pub enum Expr {
    Constant(i32),
    Unary { op: UnaryOp, expr: Box<Expr> },
}

#[derive(Debug)]
pub enum UnaryOp {
    Complement,
    Negate,
}
