use crate::{ast, tacky};

pub fn gen_tacky(ast: ast::TranslationUnit) -> tacky::TranslationUnit {
    match ast {
        ast::TranslationUnit { func} => tacky::TranslationUnit {
            func: tacky_func(func),
        }
    }
}

fn tacky_func(func: ast::Func) -> tacky::Func {
    tacky::Func {
        name: func.ident,
        instructions: tacky_stmt(func.body),
    }
}

fn tacky_stmt(stmt: ast::Stmt) -> Vec<tacky::Instruction> {
    match stmt {
        ast::Stmt::Return{expr} => {
            let (mut instrutions, value) = tacky_expr(expr);

            instrutions.push(tacky::Instruction::Return(value));

            instrutions
        }
    }
}

fn tacky_expr(expr: ast::Expr) -> (Vec<tacky::Instruction>, tacky::Val) {
    match expr {
        ast::Expr::Constant(val) => (vec![], tacky::Val::Constant(val)),
        ast::Expr::Unary{op, expr} => {
            let (instructions, inner) = tacky_expr(*expr);
            let dest_name = make_temp_var();
            let dest = tacky::Val::Var(dest_name);
            let op = tacky_unop(op);

            let mut instructions = instructions;
            instructions.push(tacky::Instruction::Unary {
                op,
                src: inner,
                dest: dest.clone(),
            });

            (instructions, dest)
        }
    }
}

fn tacky_unop(op: ast::UnaryOp) -> tacky::UnaryOp {
    match op {
        ast::UnaryOp::Complement => tacky::UnaryOp::Complement,
        ast::UnaryOp::Negate => tacky::UnaryOp::Negate
    }
}

// TODO: Change temp var to better system that includes way to provide name and other info
static mut COUNTER: i32 = 0;

fn make_temp_var() -> String {
    unsafe {
        COUNTER += 1;
        format!("tmp.{}", COUNTER)
    }
}