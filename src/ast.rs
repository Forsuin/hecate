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
pub struct Stmt {
    pub expr: Expr,
}

#[derive(Debug)]
pub struct Expr {
    pub val: i32,
}
