use ast::{BinaryOp, BlockItem, Expr};
use unique_ident::*;

use crate::tacky;
use crate::tacky::{Instruction, Val};

pub fn gen_tacky(ast: ast::TranslationUnit) -> tacky::TranslationUnit {
    match ast {
        ast::TranslationUnit { func } => tacky::TranslationUnit {
            func: tacky_func(func),
        },
    }
}

fn tacky_func(func: ast::Func) -> tacky::Func {
    let mut instructions = vec![];

    for block_item in func.body {
        match block_item {
            BlockItem::S(stmt) => {
                for instruction in tacky_stmt(stmt) {
                    instructions.push(instruction);
                }
            }
            BlockItem::D(decl) => match decl.init {
                Some(init) => {
                    let (expr_instr, _) = tacky_expr(Expr::Assignment {
                        lvalue: Box::new(Expr::Var(decl.name)),
                        expr: Box::new(init),
                    });
                    for instruction in expr_instr {
                        instructions.push(instruction);
                    }
                }
                None => {}
            },
        }
    }

    instructions.push(Instruction::Return(Val::Constant(0)));

    tacky::Func {
        name: func.ident,
        instructions,
    }
}

fn tacky_stmt(stmt: ast::Stmt) -> Vec<tacky::Instruction> {
    match stmt {
        ast::Stmt::Return { expr } => {
            let (mut instructions, value) = tacky_expr(expr);

            instructions.push(tacky::Instruction::Return(value));

            instructions
        }
        ast::Stmt::Expression { expr } => {
            let (instructions, _) = tacky_expr(expr);
            instructions
        }
        ast::Stmt::Null => {
            vec![]
        }
    }
}

fn tacky_expr(expr: ast::Expr) -> (Vec<tacky::Instruction>, tacky::Val) {
    match expr {
        ast::Expr::Constant(val) => (vec![], tacky::Val::Constant(val)),
        ast::Expr::Unary { op, expr } => {
            let (instructions, inner) = tacky_expr(*expr);
            let dest_name = make_temp();
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
            let dest_name = make_temp();
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

        Expr::Var(v) => (vec![], tacky::Val::Var(v)),
        Expr::Assignment { lvalue, expr } => {

            let v = match *lvalue {
                Expr::Var(v) => v,
                _ => unreachable!("Assignment lvalue should always be a Var"),
            };

            let (mut instructions, result) = tacky_expr(*expr);

            instructions.push(tacky::Instruction::Copy {
                src: result,
                dest: tacky::Val::Var(v.clone()),
            });

            (instructions, tacky::Val::Var(v))
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
