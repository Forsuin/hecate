use ast::BinaryOp;

use crate::tacky;

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
        ast::Expr::Binary {
            op: BinaryOp::And,
            left,
            right,
        } => {
            let (left_instr, v1) = tacky_expr(*left);
            let (right_instr, v2) = tacky_expr(*right);
            let false_label = make_label("and_false_branch");
            let end_label = make_label("and_end");
            let dest_name = make_temp();
            let dest = tacky::Val::Var(dest_name);

            let instructions = left_instr
                .into_iter()
                .chain(vec![tacky::Instruction::JumpIfZero {
                    condition: v1,
                    target: false_label.clone(),
                }])
                .chain(right_instr)
                .chain(vec![
                    tacky::Instruction::JumpIfZero {
                        condition: v2,
                        target: false_label.clone(),
                    },
                    tacky::Instruction::Copy {
                        src: tacky::Val::Constant(1),
                        dest: dest.clone(),
                    },
                    tacky::Instruction::Jump {
                        target: end_label.clone(),
                    },
                    tacky::Instruction::Label(false_label),
                    tacky::Instruction::Copy {
                        src: tacky::Val::Constant(0),
                        dest: dest.clone(),
                    },
                    tacky::Instruction::Label(end_label),
                ])
                .collect();

            (instructions, dest)
        }
        ast::Expr::Binary {
            op: BinaryOp::Or,
            left,
            right,
        } => {
            let (left_instr, v1) = tacky_expr(*left);
            let (right_instr, v2) = tacky_expr(*right);
            let true_label = make_label("or_true_branch");
            let end_label = make_label("or_end");
            let dest_name = make_temp();
            let dest = tacky::Val::Var(dest_name);

            let instructions = left_instr
                .into_iter()
                .chain(vec![tacky::Instruction::JumpIfNotZero {
                    condition: v1,
                    target: true_label.clone(),
                }])
                .chain(right_instr)
                .chain(vec![
                    tacky::Instruction::JumpIfNotZero {
                        condition: v2,
                        target: true_label.clone(),
                    },
                    tacky::Instruction::Copy {
                        src: tacky::Val::Constant(0),
                        dest: dest.clone(),
                    },
                    tacky::Instruction::Jump {
                        target: end_label.clone(),
                    },
                    tacky::Instruction::Label(true_label),
                    tacky::Instruction::Copy {
                        src: tacky::Val::Constant(1),
                        dest: dest.clone(),
                    },
                    tacky::Instruction::Label(end_label),
                ])
                .collect();

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
        ast::UnaryOp::Not => tacky::UnaryOp::Not,
    }
}

fn tacky_binop(op: ast::BinaryOp) -> tacky::BinaryOp {
    match op {
        BinaryOp::Add => tacky::BinaryOp::Add,
        BinaryOp::Subtract => tacky::BinaryOp::Subtract,
        BinaryOp::Multiply => tacky::BinaryOp::Multiply,
        BinaryOp::Divide => tacky::BinaryOp::Divide,
        BinaryOp::Modulo => tacky::BinaryOp::Modulo,

        // Logical
        BinaryOp::Equal => tacky::BinaryOp::Equal,
        BinaryOp::NotEqual => tacky::BinaryOp::NotEqual,
        BinaryOp::Less => tacky::BinaryOp::Less,
        BinaryOp::LessEqual => tacky::BinaryOp::LessEqual,
        BinaryOp::Greater => tacky::BinaryOp::Greater,
        BinaryOp::GreaterEqual => tacky::BinaryOp::GreaterEqual,

        // Bitwise
        BinaryOp::BitwiseAnd => tacky::BinaryOp::BitwiseAnd,
        BinaryOp::BitwiseOr => tacky::BinaryOp::BitwiseOr,
        BinaryOp::BitwiseXor => tacky::BinaryOp::BitwiseXor,
        BinaryOp::BitshiftLeft => tacky::BinaryOp::BitshiftLeft,
        BinaryOp::BitshiftRight => tacky::BinaryOp::BitshiftRight,

        BinaryOp::And | BinaryOp::Or => {
            panic!("Internal error, cannot convert {:?} directly to TACKY", op)
        }
    }
}

static mut VAR_COUNTER: i32 = 0;
static mut LABEL_COUNTER: i32 = 0;

fn make_temp_var() -> String {
    unsafe {
        let string = format!("tmp.{}", VAR_COUNTER);
        VAR_COUNTER += 1;
        string
    }
}

fn make_temp() -> String {
    unsafe {
        let string = format!("tmp.{}", LABEL_COUNTER);
        LABEL_COUNTER += 1;
        string
    }
}

fn make_label(prefix: &str) -> String {
    unsafe {
        let string = format!("{}.{}", prefix, LABEL_COUNTER);
        LABEL_COUNTER += 1;
        string
    }
}
