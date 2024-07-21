use crate::{Expr, Func, Function, Instruction, Operand, Program, Stmt, TranslationUnit};

pub fn gen_assm(ast: &TranslationUnit) -> Program {
    match &ast {
        &TranslationUnit{func} => Program {func: gen_func(&func)}
    }
}

pub fn gen_func(func: &Func) -> Function {
    Function {
        name: func.ident.clone(),
        instructions: gen_stmts(&func.body),
    }
}

pub fn gen_stmts(stmt: &Stmt) -> Vec<Instruction> {
    let expr = gen_expr(&stmt.expr);
    vec![Instruction::Mov {src: expr, dest: Operand::Register}, Instruction::Ret]
}

pub fn gen_expr(expr: &Expr) -> Operand {
    Operand::Imm(expr.val)
}