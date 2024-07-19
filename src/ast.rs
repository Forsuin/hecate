/// Defines AST datatypes
pub struct TranslationUnit {
    pub func: Func,
}

pub struct Func {
    pub ident: String,
    pub body: Stmt,
}

pub struct Stmt {
    pub expr: Expr,
}

pub struct Expr {
    pub val: i32,
}