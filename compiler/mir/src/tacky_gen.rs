use ast::{BinaryOp, BlockItem, Expr, ForInit, Stmt, UnaryOp};
use unique_ident::*;

use crate::tacky;
use crate::tacky::{Instruction, Val};
use crate::tacky::Instruction::{Jump, JumpIfNotZero};

pub fn gen_tacky(ast: ast::TranslationUnit) -> tacky::TranslationUnit {
    match ast {
        ast::TranslationUnit { func } => tacky::TranslationUnit {
            func: tacky_func(func),
        },
    }
}

fn tacky_func(func: ast::Func) -> tacky::Func {
    let mut instructions = vec![];

    instructions.append(&mut tacky_block(func.body));

    instructions.push(Instruction::Return(Val::Constant(0)));

    tacky::Func {
        name: func.ident,
        instructions,
    }
}

fn tacky_block(block: ast::Block) -> Vec<Instruction> {
    let mut instructions = vec![];

    for block_item in block.items {
        match block_item {
            BlockItem::S(stmt) => {
                for instruction in tacky_stmt(stmt) {
                    instructions.push(instruction);
                }
            }
            BlockItem::D(decl) => {
                instructions.append(&mut tacky_decl(decl));
            }
        }
    }

    instructions
}

fn tacky_decl(decl: ast::Decl) -> Vec<Instruction> {
    let mut instructions = vec![];

    match decl.init {
        Some(init) => {
            let (mut expr_instr, _) = tacky_expr(Expr::Assignment {
                lvalue: Box::new(Expr::Var(decl.name)),
                expr: Box::new(init),
            });
            instructions.append(&mut expr_instr);
        }
        None => {}
    }

    instructions
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
        Stmt::If { condition, then, otherwise } => {
            let else_label = make_label("else_branch");
            let end_label = make_label("end_if");

            let (mut instructions, c) = tacky_expr(condition);

            instructions.push(Instruction::JumpIfZero {
                condition: c,
                target: if otherwise.is_some() { else_label.clone() } else { end_label.clone() },
            });

            instructions.append(&mut tacky_stmt(*then));

            if let Some(otherwise) = otherwise {
                instructions.push(Jump { target: end_label.clone() });
                instructions.push(Instruction::Label(else_label.clone()));
                instructions.append(&mut tacky_stmt(*otherwise));
            }
            else {
                // Do nothing
            }

            instructions.push(Instruction::Label(end_label.clone()));

            instructions
        }
        ast::Stmt::Null => {
            vec![]
        }

        Stmt::Goto { label } => {
            vec![Instruction::Jump { target: label }]
        }
        Stmt::LabeledStmt { label, stmt } => {
            let mut instructions = vec![Instruction::Label(label)];

            instructions.append(&mut tacky_stmt(*stmt));

            instructions
        }

        Stmt::Compound { block } => {
            tacky_block(block)
        }

        Stmt::Break { label } => {
            let mut instructions = vec![];

            instructions.push(Instruction::Jump {
                target: format!("break.{}", label)
            });

            instructions
        }
        Stmt::Continue { label } => {
            let mut instructions = vec![];

            instructions.push(Instruction::Jump {
                target: format!("continue.{}", label)
            });

            instructions
        }
        Stmt::While { condition, body, label } => {
            let mut instuctions = vec![];

            let continue_label = format!("continue.{}", label);
            let break_label = format!("break.{}", label);

            let (mut cond_instr, cond) = tacky_expr(condition);

            instuctions.push(Instruction::Label(continue_label.clone()));
            instuctions.append(&mut cond_instr);
            instuctions.push(Instruction::JumpIfZero {
                condition: cond,
                target: break_label.clone()
            });

            instuctions.append(&mut tacky_stmt(*body));

            instuctions.push(Instruction::Jump {
                target: continue_label.clone()
            });

            instuctions.push(Instruction::Label(break_label));

            instuctions

        }
        Stmt::DoWhile { body, condition, label } => {
            let mut instructions = vec![];
            let start_label = make_label("do_start_loop");
            let continue_label = format!("continue.{}", label);
            let break_label = format!("break.{}", label);

            instructions.push(Instruction::Label(start_label.clone()));
            instructions.append(&mut tacky_stmt(*body));
            instructions.push(Instruction::Label(continue_label));

            let (mut cond_instr, cond) = tacky_expr(condition);

            instructions.append(&mut cond_instr);
            instructions.push(Instruction::JumpIfNotZero {
                condition: cond,
                target: start_label,
            });
            instructions.push(Instruction::Label(break_label));

            instructions
        }
        Stmt::For { init, condition, post, body, label } => {
            let mut instructions = vec![];
            let start_label = make_label("for_start_loop");
            let continue_label = format!("continue.{}", label);
            let break_label = format!("break.{}", label);

            match init {
                ForInit::Decl(decl) => {
                    instructions.append(&mut tacky_decl(decl));
                }
                ForInit::Expr(expr) => {
                    match expr {
                        Some(expr) => {
                            instructions.append(&mut tacky_expr(expr).0);
                        }
                        None => {}
                    }
                }
            }

            instructions.push(Instruction::Label(start_label.clone()));

            match condition {
                Some(expr) => {
                    let (mut cond_instr, cond) = tacky_expr(expr);

                    instructions.append(&mut cond_instr);

                    instructions.push(Instruction::JumpIfZero {
                        condition: cond,
                        target: break_label.clone(),
                    });
                }
                None => {}
            }

            instructions.append(&mut tacky_stmt(*body));

            instructions.push(Instruction::Label(continue_label.clone()));

            match post {
                Some(expr) => {
                    let (mut post_instr, _) = tacky_expr(expr);

                    instructions.append(&mut post_instr);
                }
                None => {}
            }

            instructions.push(Instruction::Jump {
                target: start_label.clone()
            });

            instructions.push(Instruction::Label(break_label));

            instructions
        }
        Stmt::Switch { control, body, label } => {
            tacky_switch(control, *body, label)
        }
        Stmt::Case { constant: _, body, label } => {
            let mut instructions = vec![];

            instructions.push(Instruction::Label(label));

            instructions.append(& mut tacky_stmt(*body));

            instructions
        }
        Stmt::Default { body, label } => {
            let mut instructions = vec![];

            instructions.push(Instruction::Label(label));

            instructions.append(& mut tacky_stmt(*body));

            instructions
        }
    }

}

fn tacky_expr(expr: ast::Expr) -> (Vec<tacky::Instruction>, tacky::Val) {
    match expr {
        ast::Expr::Constant(val) => (vec![], tacky::Val::Constant(val)),
        Expr::Unary { op: UnaryOp::Inc, expr } => {
            tacky_compound_expression(BinaryOp::Add, *expr, Expr::Constant(1))
        }
        Expr::Unary { op: UnaryOp::Dec, expr } => {
            tacky_compound_expression(BinaryOp::Subtract, *expr, Expr::Constant(1))
        }
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
        Expr::CompoundAssignment { op, lvalue, expr } => tacky_compound_expression(op, *lvalue, *expr),
        Expr::PostfixInc(expr) => {
            let expr = match *expr {
                Expr::Var(v) => Val::Var(v),
                _ => unreachable!("Assignment lvalue should always be a Var"),
            };

            let dest = Val::Var(make_temp());
            
            let instructions = vec![
                Instruction::Copy { src: expr.clone(), dest: dest.clone() },
                Instruction::Binary {
                    op: tacky_binop(BinaryOp::Add),
                    first: expr.clone(),
                    second: Val::Constant(1),
                    dest: expr.clone(),
                }
            ];

            (instructions, dest)
        }
        Expr::PostfixDec(expr) => {
            let expr = match *expr {
                Expr::Var(v) => Val::Var(v),
                _ => unreachable!("Assignment lvalue should always be a Var"),
            };

            let dest = Val::Var(make_temp());

            let instructions = vec![
                Instruction::Copy { src: expr.clone(), dest: dest.clone() },
                Instruction::Binary {
                    op: tacky_binop(BinaryOp::Subtract),
                    first: expr.clone(),
                    second: Val::Constant(1),
                    dest: expr.clone(),
                }
            ];

            (instructions, dest)
        }
        Expr::Conditional { condition, then, otherwise } => {
            let else_label = make_label("else_branch");
            let end_label = make_label("end_if");
            let result_name = make_temp();
            let result = Val::Var(result_name);

            let (mut instructions, c) = tacky_expr(*condition);

            instructions.push(Instruction::JumpIfZero {
                condition: c,
                target: else_label.clone(),
            });

            let (mut then_instructions, v1) = tacky_expr(*then);
            instructions.append(&mut then_instructions);

            instructions.push(Instruction::Copy {
                src: v1,
                dest: result.clone(),
            });
            instructions.push(Instruction::Jump {
               target: end_label.clone()
            });

            instructions.push(Instruction::Label(else_label));

            let (mut else_instructions, v2) = tacky_expr(*otherwise);
            instructions.append(&mut else_instructions);

            instructions.push(Instruction::Copy {
                src: v2,
                dest: result.clone(),
            });

            instructions.push(Instruction::Label(end_label));

            (instructions, result)
        }
    }
}

fn tacky_unop(op: ast::UnaryOp) -> tacky::UnaryOp {
    match op {
        ast::UnaryOp::Complement => tacky::UnaryOp::Complement,
        ast::UnaryOp::Negate => tacky::UnaryOp::Negate,
        ast::UnaryOp::Not => tacky::UnaryOp::Not,
        _ => unreachable!("Inc and Dec shouldn't be handled here")
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

fn tacky_compound_expression(op: BinaryOp, lvalue: Expr, rhs: Expr) -> (Vec<Instruction>, Val) {
    let (mut instructions, rhs) = tacky_expr(rhs);

    let dest= match lvalue {
        Expr::Var(v) => Val::Var(v),
        _ => unreachable!("Assignment lvalue should always be a Var"),
    };

    let op = tacky_binop(op);

    instructions.push(Instruction::Binary {
        op,
        first: dest.clone(),
        second: rhs,
        dest: dest.clone(),
    });

    (instructions, dest)
}

fn tacky_switch(control: Expr, body: Stmt, label: String) -> Vec<Instruction> {
    let cases = gather_cases(&body);

    let break_label = format!("break.{}", label);
    let (control_instr, control) = tacky_expr(control);
    let cmp_result = Val::Var(make_temp());

    let gen_tacky_case_jump = |(key, label): &(Option<i32>, String) | {
        match key {
            Some(c) => vec![
                Instruction::Binary {
                    op: tacky::BinaryOp::Equal,
                    first: Val::Constant(*c),
                    second: control.clone(),
                    dest: cmp_result.clone(),
                },
                JumpIfNotZero {
                    condition: cmp_result.clone(),
                    target: label.clone(),
                }
            ],
            None => vec![]
        }
    };

    let tacky_case_jumps: Vec<Instruction> = cases.iter().flat_map(gen_tacky_case_jump).collect();

    let tacky_default_jump = if cases.iter().any(|(key, _)| key.is_none()) {
        let default_id = cases.iter()
            .find(|(key, _) | key.is_none())
            .map(|(_, id)| id.clone())
            .expect("Default case label not found");
        vec![Jump {
            target: default_id,
        }]
    }
    else {
        vec![]
    };

    control_instr.into_iter()
        .chain(tacky_case_jumps)
        .chain(tacky_default_jump)
        .chain(vec![Jump {
            target: break_label.clone(),
        }])
        .chain(tacky_stmt(body))
        .chain(vec![Instruction::Label(break_label)])
        .collect()
}

fn gather_cases(stmt: &Stmt) -> Vec<(Option<i32>, String)> {
    let mut cases = vec![];

    gather_cases_helper(stmt, &mut cases);

    cases
}

fn gather_cases_helper(stmt: &Stmt, cases: &mut Vec<(Option<i32>, String)>) {
    match stmt {
        Stmt::Compound { block } => {
            for item in &block.items {
                match item {
                    BlockItem::S(stmt) => gather_cases_helper(stmt, cases),
                    _ => {}
                }
            }
        }
        Stmt::If { condition: _, then, otherwise } => {
            gather_cases_helper(then, cases);

            if let Some(otherwise) = otherwise {
                gather_cases_helper(otherwise, cases);
            }
        }

        Stmt::LabeledStmt { label: _, stmt } => {
            gather_cases_helper(stmt, cases);
        }

        Stmt::While { condition: _, body, label: _ } => {
            gather_cases_helper(body, cases);
        }
        Stmt::DoWhile { body, condition: _, label: _ } => {
            gather_cases_helper(body, cases);
        }
        Stmt::For { init: _, condition: _, post: _, body, label: _ } => {
            gather_cases_helper(body, cases);
        }

        Stmt::Case { constant, body, label } => {
            if let Expr::Constant(c) = constant {
                cases.push((Some(*c), label.clone()));
                gather_cases_helper(body, cases);
            }
            else {
                unreachable!("Tacky Gen, case expr is non-constant value");
            }
        }
        Stmt::Default { body: _, label } => {
            cases.push((None, label.clone()));
        }
        Stmt::Switch { .. } | Stmt::Continue { .. } | Stmt::Break { .. } | Stmt::Goto { .. } | Stmt::Expression { .. } | Stmt::Return { .. } | Stmt::Null => {}
    }
}