use ast::{BinaryOp, BlockItem, Decl, Expr, ExprKind, ForInit, Stmt, UnaryOp};
use ty::{
    const_convert, get_size, is_signed, Constant, IdentifierAttr, InitialVal, StaticInit,
    SymbolTable, Type,
};
use unique_ident::*;

use crate::tacky;
use crate::tacky::Instruction::{Jump, JumpIfNotZero};
use crate::tacky::*;

pub fn gen_tacky(ast: &ast::TranslationUnit, symbols: &mut SymbolTable) -> tacky::TranslationUnit {
    let mut decls: Vec<tacky::Decl> = vec![];

    for decl in &ast.decls {
        match decl {
            Decl::FuncDecl(decl) => {
                if decl.body.is_some() {
                    decls.push(tacky::Decl::Func(tacky_func(decl, symbols)));
                }
            }
            Decl::VarDecl(_) => {}
        }
    }

    decls.append(&mut tacky_symbol_table(symbols));

    TranslationUnit { decls }
}

fn tacky_symbol_table(symbols: &SymbolTable) -> Vec<tacky::Decl> {
    let mut vars = vec![];

    for (name, entry) in &symbols.symbols {
        if let IdentifierAttr::Static { init, global } = &entry.attrs {
            match init {
                InitialVal::Tentative => vars.push(tacky::Decl::StaticVar(StaticVar {
                    name: name.clone(),
                    global: *global,
                    ty: entry.t.clone(),
                    init: match entry.t {
                        Type::Int => StaticInit::Int(0),
                        Type::Long => StaticInit::Long(0),
                        Type::UInt => StaticInit::UInt(0),
                        Type::ULong => StaticInit::ULong(0),
                        Type::Func(_) => unreachable!(
                            "Internal Error: Should never have function with a tentative value"
                        ),
                    },
                })),
                InitialVal::Initial(i) => vars.push(tacky::Decl::StaticVar(StaticVar {
                    name: name.clone(),
                    global: *global,
                    ty: entry.t.clone(),
                    init: i.clone(),
                })),
                InitialVal::NoInit => {}
            }
        }
    }

    vars
}

fn tacky_func(func: &ast::FuncDecl, symbols: &mut SymbolTable) -> tacky::Func {
    let mut instructions = vec![];

    if let Some(body) = &func.body {
        instructions.append(&mut tacky_block(body, symbols));
    }

    instructions.push(Instruction::Return(Val::Constant(Constant::Int(0))));

    tacky::Func {
        name: func.ident.clone(),
        params: func.params.clone(),
        instructions,
        global: symbols.is_global(&func.ident),
    }
}

fn tacky_block(block: &ast::Block, symbols: &mut SymbolTable) -> Vec<Instruction> {
    let mut instructions = vec![];

    for block_item in &block.items {
        match block_item {
            BlockItem::S(stmt) => {
                for instruction in tacky_stmt(stmt, symbols) {
                    instructions.push(instruction);
                }
            }
            BlockItem::D(decl) => {
                instructions.append(&mut tacky_decl(decl, symbols));
            }
        }
    }

    instructions
}

fn tacky_decl(decl: &ast::Decl, symbols: &mut SymbolTable) -> Vec<Instruction> {
    match decl {
        Decl::FuncDecl(_) => {
            vec![]
        }
        Decl::VarDecl(ast::VarDecl {
            storage_class: Some(_),
            ..
        }) => {
            vec![]
        }
        Decl::VarDecl(var) => tacky_var_decl(var, symbols),
    }
}

fn tacky_var_decl(decl: &ast::VarDecl, symbols: &mut SymbolTable) -> Vec<Instruction> {
    let mut instructions = vec![];

    match &decl.init {
        Some(init) => {
            let (mut expr_instr, _) = tacky_expr(
                &Expr::new(ExprKind::Assignment {
                    lvalue: Box::new(Expr::new(ExprKind::Var(decl.name.clone()))),
                    expr: Box::new(init.clone()),
                }),
                symbols,
            );
            instructions.append(&mut expr_instr);
        }
        None => {}
    }

    instructions
}

fn tacky_stmt(stmt: &ast::Stmt, symbols: &mut SymbolTable) -> Vec<tacky::Instruction> {
    match stmt {
        ast::Stmt::Return { expr } => {
            let (mut instructions, value) = tacky_expr(expr, symbols);

            instructions.push(tacky::Instruction::Return(value));

            instructions
        }
        ast::Stmt::Expression { expr } => {
            let (instructions, _) = tacky_expr(expr, symbols);
            instructions
        }
        Stmt::If {
            condition,
            then,
            otherwise,
        } => {
            let else_label = make_label("else_branch");
            let end_label = make_label("end_if");

            let (mut instructions, c) = tacky_expr(condition, symbols);

            instructions.push(Instruction::JumpIfZero {
                condition: c,
                target: if otherwise.is_some() {
                    else_label.clone()
                } else {
                    end_label.clone()
                },
            });

            instructions.append(&mut tacky_stmt(then, symbols));

            if let Some(otherwise) = otherwise {
                instructions.push(Jump {
                    target: end_label.clone(),
                });
                instructions.push(Instruction::Label(else_label.clone()));
                instructions.append(&mut tacky_stmt(otherwise, symbols));
            } else {
                // Do nothing
            }

            instructions.push(Instruction::Label(end_label.clone()));

            instructions
        }
        ast::Stmt::Null => {
            vec![]
        }

        Stmt::Goto { label } => {
            vec![Instruction::Jump {
                target: label.clone(),
            }]
        }
        Stmt::LabeledStmt { label, stmt } => {
            let mut instructions = vec![Instruction::Label(label.clone())];

            instructions.append(&mut tacky_stmt(stmt, symbols));

            instructions
        }

        Stmt::Compound { block } => tacky_block(block, symbols),

        Stmt::Break { label } => {
            let mut instructions = vec![];

            instructions.push(Instruction::Jump {
                target: format!("break.{}", label),
            });

            instructions
        }
        Stmt::Continue { label } => {
            let mut instructions = vec![];

            instructions.push(Instruction::Jump {
                target: format!("continue.{}", label),
            });

            instructions
        }
        Stmt::While {
            condition,
            body,
            label,
        } => {
            let mut instuctions = vec![];

            let continue_label = format!("continue.{}", label);
            let break_label = format!("break.{}", label);

            let (mut cond_instr, cond) = tacky_expr(condition, symbols);

            instuctions.push(Instruction::Label(continue_label.clone()));
            instuctions.append(&mut cond_instr);
            instuctions.push(Instruction::JumpIfZero {
                condition: cond,
                target: break_label.clone(),
            });

            instuctions.append(&mut tacky_stmt(body, symbols));

            instuctions.push(Instruction::Jump {
                target: continue_label.clone(),
            });

            instuctions.push(Instruction::Label(break_label));

            instuctions
        }
        Stmt::DoWhile {
            body,
            condition,
            label,
        } => {
            let mut instructions = vec![];
            let start_label = make_label("do_start_loop");
            let continue_label = format!("continue.{}", label);
            let break_label = format!("break.{}", label);

            instructions.push(Instruction::Label(start_label.clone()));
            instructions.append(&mut tacky_stmt(body, symbols));
            instructions.push(Instruction::Label(continue_label));

            let (mut cond_instr, cond) = tacky_expr(condition, symbols);

            instructions.append(&mut cond_instr);
            instructions.push(Instruction::JumpIfNotZero {
                condition: cond,
                target: start_label,
            });
            instructions.push(Instruction::Label(break_label));

            instructions
        }
        Stmt::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let mut instructions = vec![];
            let start_label = make_label("for_start_loop");
            let continue_label = format!("continue.{}", label);
            let break_label = format!("break.{}", label);

            match init {
                ForInit::Decl(decl) => {
                    instructions.append(&mut tacky_var_decl(decl, symbols));
                }
                ForInit::Expr(expr) => match expr {
                    Some(expr) => {
                        instructions.append(&mut tacky_expr(expr, symbols).0);
                    }
                    None => {}
                },
            }

            instructions.push(Instruction::Label(start_label.clone()));

            match condition {
                Some(expr) => {
                    let (mut cond_instr, cond) = tacky_expr(expr, symbols);

                    instructions.append(&mut cond_instr);

                    instructions.push(Instruction::JumpIfZero {
                        condition: cond,
                        target: break_label.clone(),
                    });
                }
                None => {}
            }

            instructions.append(&mut tacky_stmt(body, symbols));

            instructions.push(Instruction::Label(continue_label.clone()));

            match post {
                Some(expr) => {
                    let (mut post_instr, _) = tacky_expr(expr, symbols);

                    instructions.append(&mut post_instr);
                }
                None => {}
            }

            instructions.push(Instruction::Jump {
                target: start_label.clone(),
            });

            instructions.push(Instruction::Label(break_label));

            instructions
        }
        Stmt::Switch {
            control,
            body,
            label,
        } => tacky_switch(control, body, label.clone(), symbols),
        Stmt::Case {
            constant: _,
            body,
            label,
        } => {
            let mut instructions = vec![];

            instructions.push(Instruction::Label(label.clone()));

            instructions.append(&mut tacky_stmt(body, symbols));

            instructions
        }
        Stmt::Default { body, label } => {
            let mut instructions = vec![];

            instructions.push(Instruction::Label(label.clone()));

            instructions.append(&mut tacky_stmt(body, symbols));

            instructions
        }
    }
}

fn tacky_expr(
    expr: &ast::Expr,
    symbols: &mut SymbolTable,
) -> (Vec<tacky::Instruction>, tacky::Val) {
    match &expr.kind {
        ast::ExprKind::Constant(val) => (vec![], tacky::Val::Constant(val.clone())),
        ExprKind::Unary {
            op: UnaryOp::Inc,
            expr,
        } => tacky_compound_expression(
            BinaryOp::Add,
            expr,
            &Expr::new(ExprKind::Constant(Constant::Int(1))),
            expr.get_type().unwrap(),
            symbols,
        ),
        ExprKind::Unary {
            op: UnaryOp::Dec,
            expr,
        } => tacky_compound_expression(
            BinaryOp::Subtract,
            expr,
            &Expr::new(ExprKind::Constant(Constant::Int(1))),
            expr.get_type().unwrap(),
            symbols,
        ),
        ast::ExprKind::Unary { op, expr } => {
            let (instructions, inner) = tacky_expr(expr, symbols);
            let dest_name = tacky_temp(expr.get_type().unwrap(), symbols);
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
        ast::ExprKind::Binary {
            op: BinaryOp::And,
            left,
            right,
        } => {
            let (left_instr, v1) = tacky_expr(left, symbols);
            let (right_instr, v2) = tacky_expr(right, symbols);
            let false_label = make_label("and_false_branch");
            let end_label = make_label("and_end");
            let dest_name = tacky_temp(expr.get_type().unwrap(), symbols);
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
                        src: tacky::Val::Constant(Constant::Int(1)),
                        dest: dest.clone(),
                    },
                    tacky::Instruction::Jump {
                        target: end_label.clone(),
                    },
                    tacky::Instruction::Label(false_label),
                    tacky::Instruction::Copy {
                        src: tacky::Val::Constant(Constant::Int(0)),
                        dest: dest.clone(),
                    },
                    tacky::Instruction::Label(end_label),
                ])
                .collect();

            (instructions, dest)
        }
        ast::ExprKind::Binary {
            op: BinaryOp::Or,
            left,
            right,
        } => {
            let (left_instr, v1) = tacky_expr(left, symbols);
            let (right_instr, v2) = tacky_expr(right, symbols);
            let true_label = make_label("or_true_branch");
            let end_label = make_label("or_end");
            let dest_name = tacky_temp(expr.get_type().unwrap(), symbols);
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
                        src: tacky::Val::Constant(Constant::Int(0)),
                        dest: dest.clone(),
                    },
                    tacky::Instruction::Jump {
                        target: end_label.clone(),
                    },
                    tacky::Instruction::Label(true_label),
                    tacky::Instruction::Copy {
                        src: tacky::Val::Constant(Constant::Int(1)),
                        dest: dest.clone(),
                    },
                    tacky::Instruction::Label(end_label),
                ])
                .collect();

            (instructions, dest)
        }
        ExprKind::Binary { op, left, right } => {
            let (left_instr, left_inner) = tacky_expr(left, symbols);
            let (mut right_instr, right_inner) = tacky_expr(right, symbols);
            let dest_name = tacky_temp(expr.get_type().unwrap(), symbols);
            let dest = Val::Var(dest_name);
            let op = tacky_binop(*op);

            let mut instructions = left_instr;
            instructions.append(&mut right_instr);
            instructions.push(Instruction::Binary {
                op,
                first: left_inner,
                second: right_inner,
                dest: dest.clone(),
            });

            (instructions, dest)
        }

        ExprKind::Var(v) => (vec![], Val::Var(v.clone())),
        ExprKind::Assignment { lvalue, expr } => {
            let v = match &lvalue.kind {
                ExprKind::Var(v) => v,
                _ => unreachable!("Assignment lvalue should always be a Var"),
            };

            let (mut instructions, result) = tacky_expr(expr, symbols);

            instructions.push(tacky::Instruction::Copy {
                src: result,
                dest: Val::Var(v.clone()),
            });

            (instructions, Val::Var(v.clone()))
        }
        ExprKind::CompoundAssignment {
            op,
            lvalue,
            expr: rhs,
        } => tacky_compound_expression(*op, lvalue, rhs, expr.get_type().unwrap(), symbols),
        ExprKind::PostfixInc(expr) => {
            let val = match &expr.kind {
                ExprKind::Var(v) => Val::Var(v.clone()),
                _ => unreachable!("Assignment lvalue should always be a Var"),
            };

            let dest = Val::Var(tacky_temp(expr.get_type().unwrap(), symbols));

            let instructions = vec![
                Instruction::Copy {
                    src: val.clone(),
                    dest: dest.clone(),
                },
                Instruction::Binary {
                    op: tacky_binop(BinaryOp::Add),
                    first: val.clone(),
                    second: Val::Constant(make_constant(&expr.get_type().unwrap(), 1)),
                    dest: val.clone(),
                },
            ];

            (instructions, dest)
        }
        ExprKind::PostfixDec(expr) => {
            let val = match &expr.kind {
                ExprKind::Var(v) => Val::Var(v.clone()),
                _ => unreachable!("Assignment lvalue should always be a Var"),
            };

            let dest = Val::Var(tacky_temp(expr.get_type().unwrap(), symbols));

            let instructions = vec![
                Instruction::Copy {
                    src: val.clone(),
                    dest: dest.clone(),
                },
                Instruction::Binary {
                    op: tacky_binop(BinaryOp::Subtract),
                    first: val.clone(),
                    second: Val::Constant(make_constant(&expr.get_type().unwrap(), 1)),
                    dest: val.clone(),
                },
            ];

            (instructions, dest)
        }
        ExprKind::Conditional {
            condition,
            then,
            otherwise,
        } => {
            let else_label = make_label("else_branch");
            let end_label = make_label("end_if");
            let result_name = tacky_temp(expr.get_type().unwrap(), symbols);
            let result = Val::Var(result_name);

            let (mut instructions, c) = tacky_expr(condition, symbols);

            instructions.push(Instruction::JumpIfZero {
                condition: c,
                target: else_label.clone(),
            });

            let (mut then_instructions, v1) = tacky_expr(then, symbols);
            instructions.append(&mut then_instructions);

            instructions.push(Instruction::Copy {
                src: v1,
                dest: result.clone(),
            });
            instructions.push(Instruction::Jump {
                target: end_label.clone(),
            });

            instructions.push(Instruction::Label(else_label));

            let (mut else_instructions, v2) = tacky_expr(otherwise, symbols);
            instructions.append(&mut else_instructions);

            instructions.push(Instruction::Copy {
                src: v2,
                dest: result.clone(),
            });

            instructions.push(Instruction::Label(end_label));

            (instructions, result)
        }
        ExprKind::FunctionCall { func, args } => {
            let dest_name = tacky_temp(expr.get_type().unwrap(), symbols);
            let dest = Val::Var(dest_name);

            let mut arg_instr = vec![];
            let mut arg_vals = vec![];

            for arg in args {
                let (mut instr, arg_val) = tacky_expr(arg, symbols);
                arg_instr.append(&mut instr);
                arg_vals.push(arg_val);
            }

            arg_instr.push(Instruction::FunCall {
                func_name: func.clone(),
                args: arg_vals,
                dest: dest.clone(),
            });

            (arg_instr, dest)
        }
        ExprKind::Cast {
            target_type,
            expr: inner,
        } => {
            let (mut instructions, result) = tacky_expr(inner, symbols);
            let inner_type = inner.get_type().unwrap();

            if inner_type == *target_type {
                (instructions, result)
            } else {
                let dest_name = tacky_temp(target_type.clone(), symbols);
                let dest = Val::Var(dest_name);

                let cast_instructions =
                    get_cast_instruction(result.clone(), dest.clone(), &inner_type, target_type);

                instructions.push(cast_instructions);
                (instructions, dest)
            }
        }
    }
}

fn make_constant(ty: &Type, i: i32) -> Constant {
    let as_int = Constant::Int(i);
    const_convert(ty, as_int)
}

fn tacky_temp(var_type: Type, symbols: &mut SymbolTable) -> String {
    let name = make_temp();
    symbols.add_automatic_var(name.clone(), var_type);
    name
}

fn get_cast_instruction(src: Val, dest: Val, inner_type: &Type, target_type: &Type) -> Instruction {
    if get_size(target_type) == get_size(&inner_type) {
        Instruction::Copy { src, dest }
    } else if get_size(target_type) < get_size(&inner_type) {
        Instruction::Truncate { src, dest }
    } else if is_signed(&inner_type) {
        Instruction::SignExtend { src, dest }
    } else {
        Instruction::ZeroExtend { src, dest }
    }
}

fn tacky_unop(op: &ast::UnaryOp) -> tacky::UnaryOp {
    match op {
        ast::UnaryOp::Complement => tacky::UnaryOp::Complement,
        ast::UnaryOp::Negate => tacky::UnaryOp::Negate,
        ast::UnaryOp::Not => tacky::UnaryOp::Not,
        _ => unreachable!("Inc and Dec shouldn't be handled here"),
    }
}

fn tacky_binop(op: BinaryOp) -> tacky::BinaryOp {
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

fn tacky_compound_expression(
    op: BinaryOp,
    lvalue: &Expr,
    rhs: &Expr,
    result_type: Type,
    symbols: &mut SymbolTable,
) -> (Vec<Instruction>, Val) {
    println!("{:#?}", lvalue);

    let dest = match &lvalue.kind {
        ExprKind::Var(v) => Val::Var(v.clone()),
        _ => unreachable!("Assignment lvalue should always be a Var"),
    };

    let (mut instructions, rhs) = tacky_expr(rhs, symbols);

    let op = tacky_binop(op);

    let mut op_and_assignment = if result_type == lvalue.get_type().unwrap() {
        // result of binary op already has correct type
        vec![Instruction::Binary {
            op: op.clone(),
            first: dest.clone(),
            second: rhs.clone(),
            dest: dest.clone(),
        }]
    } else {
        // must convert lhs to op type, then convert result back
        // tmp = (result_type) dest
        // tmp = tmp op rhs
        // lhs (lhs.type) tmp

        let temp = Val::Var(tacky_temp(result_type.clone(), symbols));
        let cast_lhs_to_temp =
            get_cast_instruction(dest.clone(), temp.clone(), &lvalue.get_type().unwrap(), &result_type);
        let bin_instr = Instruction::Binary {
            op: op.clone(),
            first: temp.clone(),
            second: rhs.clone(),
            dest: temp.clone(),
        };

        let cast_temp_to_lhs =
            get_cast_instruction(temp.clone(), dest.clone(), &result_type, &lvalue.get_type().unwrap());

        vec![cast_lhs_to_temp, bin_instr, cast_temp_to_lhs]
    };

    instructions.append(&mut op_and_assignment);

    (instructions, dest)
}

fn tacky_switch(
    control: &Expr,
    body: &Stmt,
    label: String,
    symbols: &mut SymbolTable,
) -> Vec<Instruction> {
    let cases = gather_cases(body);

    let break_label = format!("break.{}", label);
    let (mut control_instr, control_val) = tacky_expr(control, symbols);
    let cmp_result = Val::Var(tacky_temp(control.get_type().unwrap(), symbols));

    let gen_tacky_case_jump = |(key, label): &(Option<Constant>, String)| match key {
        Some(c) => {
            vec![
                Instruction::Binary {
                    op: tacky::BinaryOp::Equal,
                    first: Val::Constant(c.clone()),
                    second: control_val.clone(),
                    dest: cmp_result.clone(),
                },
                JumpIfNotZero {
                    condition: cmp_result.clone(),
                    target: label.clone(),
                },
            ]
        }
        None => vec![],
    };

    let mut tacky_case_jumps: Vec<Instruction> =
        cases.iter().flat_map(gen_tacky_case_jump).collect();

    let mut tacky_default_jump = if cases.iter().any(|(key, _)| key.is_none()) {
        let default_id = cases
            .iter()
            .find(|(key, _)| key.is_none())
            .map(|(_, id)| id.clone())
            .expect("Default case label not found");
        vec![Jump { target: default_id }]
    } else {
        vec![]
    };

    control_instr.append(&mut tacky_case_jumps);
    control_instr.append(&mut tacky_default_jump);
    control_instr.push(Instruction::Jump {
        target: break_label.clone(),
    });
    control_instr.append(&mut tacky_stmt(body, symbols));
    control_instr.push(Instruction::Label(break_label));
    control_instr
}

fn gather_cases(stmt: &Stmt) -> Vec<(Option<Constant>, String)> {
    let mut cases = vec![];

    gather_cases_helper(stmt, &mut cases);

    cases
}

fn gather_cases_helper(stmt: &Stmt, cases: &mut Vec<(Option<Constant>, String)>) {
    match stmt {
        Stmt::Compound { block } => {
            for item in &block.items {
                if let BlockItem::S(stmt) = item {
                    gather_cases_helper(stmt, cases)
                }
            }
        }
        Stmt::If {
            condition: _,
            then,
            otherwise,
        } => {
            gather_cases_helper(then, cases);

            if let Some(otherwise) = otherwise {
                gather_cases_helper(otherwise, cases);
            }
        }

        Stmt::LabeledStmt { label: _, stmt } => {
            gather_cases_helper(stmt, cases);
        }

        Stmt::While {
            condition: _,
            body,
            label: _,
        } => {
            gather_cases_helper(body, cases);
        }
        Stmt::DoWhile {
            body,
            condition: _,
            label: _,
        } => {
            gather_cases_helper(body, cases);
        }
        Stmt::For {
            init: _,
            condition: _,
            post: _,
            body,
            label: _,
        } => {
            gather_cases_helper(body, cases);
        }

        Stmt::Case {
            constant,
            body,
            label,
        } => {
            if let ExprKind::Constant(c) = &constant.kind {
                // convert the constant values to the same type as enclosing switch's control expr
                // switch_analysis stage handles typing cases before this
                let constant = const_convert(&constant.get_type().unwrap(), c.clone());

                cases.push((Some(constant), label.clone()));
                gather_cases_helper(body, cases);
            } else {
                unreachable!("Internal Error: case expr is non-constant value");
            }
        }
        Stmt::Default { body: _, label } => {
            cases.push((None, label.clone()));
        }
        Stmt::Switch { .. }
        | Stmt::Continue { .. }
        | Stmt::Break { .. }
        | Stmt::Goto { .. }
        | Stmt::Expression { .. }
        | Stmt::Return { .. }
        | Stmt::Null => {}
    }
}
