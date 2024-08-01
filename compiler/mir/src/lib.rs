use ast::BinaryOp;

pub mod tacky;

pub fn gen_tacky(ast: ast::TranslationUnit) -> tacky::TranslationUnit {
    match ast {
        ast::TranslationUnit { func } => tacky::TranslationUnit {
            func: tacky_func(func),
        },
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
        ast::Stmt::Return { expr } => {
            let (mut instrutions, value) = tacky_expr(expr);

            instrutions.push(tacky::Instruction::Return(value));

            instrutions
        }
    }
}

fn tacky_expr(expr: ast::Expr) -> (Vec<tacky::Instruction>, tacky::Val) {
    match expr {
        ast::Expr::Constant(val) => (vec![], tacky::Val::Constant(val)),
        ast::Expr::Unary { op, expr } => {
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
        ast::Expr::Binary { op, left, right } => {
            let (left_instr, left_inner) = tacky_expr(*left);
            let (mut right_instr, right_inner) = tacky_expr(*right);
            let dest_name = make_temp_var();
            let dest = tacky::Val::Var(dest_name);
            let op = tacky_binop(op);

            let mut instructions = left_instr;
            instructions.append(&mut right_instr);
            instructions.push(tacky::Instruction::Binary {
                op,
                first: left_inner,
                second: right_inner,
                dest: dest.clone(),
            });

            (instructions, dest)
        }
    }
}

fn tacky_unop(op: ast::UnaryOp) -> tacky::UnaryOp {
    match op {
        ast::UnaryOp::Complement => tacky::UnaryOp::Complement,
        ast::UnaryOp::Negate => tacky::UnaryOp::Negate,
    }
}

fn tacky_binop(op: ast::BinaryOp) -> tacky::BinaryOp {
    match op {
        BinaryOp::Add => tacky::BinaryOp::Add,
        BinaryOp::Subtract => tacky::BinaryOp::Subtract,
        BinaryOp::Multiply => tacky::BinaryOp::Multiply,
        BinaryOp::Divide => tacky::BinaryOp::Divide,
        BinaryOp::Modulo => tacky::BinaryOp::Modulo,
        BinaryOp::BitwiseAnd => tacky::BinaryOp::BitwiseAnd,
        BinaryOp::BitwiseOr => tacky::BinaryOp::BitwiseOr,
        BinaryOp::BitwiseXor => tacky::BinaryOp::BitwiseXor,
        BinaryOp::BitshiftLeft => tacky::BinaryOp::BitshiftLeft,
        BinaryOp::BitshiftRight => tacky::BinaryOp::BitshiftRight,
    }
}

static mut COUNTER: i32 = 0;

fn make_temp_var() -> String {
    unsafe {
        let string = format!("tmp.{}", COUNTER);
        COUNTER += 1;
        string
    }
}