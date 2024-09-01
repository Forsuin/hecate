use std::collections::HashMap;

use ast::*;
use unique_ident::make_temp_name;

// var's original name -> (unique_name, from_current_block)

struct VarEntry {
    pub unique_name: String,
    pub from_current_block: bool,
}

type VarMap = HashMap<String, VarEntry>;

pub fn resolve(program: &TranslationUnit) -> TranslationUnit {
    TranslationUnit {
        func: resolve_func(&program.func),
    }
}

fn resolve_func(func: &Func) -> Func {
    let mut var_map = VarMap::new();

    let resolved_body = resolve_block(&func.body, &mut var_map);

    Func {
        ident: func.ident.clone(),
        body: resolved_body,
    }
}

fn resolve_block(block: &Block, var_map: &mut VarMap) -> Block {
    let mut resolved_items = vec![];

    for block_item in &block.items {
        resolved_items.push(resolve_block_item(block_item, var_map))
    }

    Block {
        items: resolved_items,
    }
}

fn resolve_block_item(item: &BlockItem, var_map: &mut VarMap) -> BlockItem {
    match item {
        BlockItem::S(stmt) => BlockItem::S(resolve_stmt(stmt, var_map)),
        BlockItem::D(decl) => BlockItem::D(resolve_decl(decl, var_map)),
    }
}

fn resolve_decl(decl: &Decl, var_map: &mut VarMap) -> Decl {
    if var_map.contains_key(&decl.name) && var_map.get(&decl.name).unwrap().from_current_block {
        panic!("Duplicate variable declaration");
    } else {
        let unique_name = make_temp_name(&decl.name);
        var_map.insert(
            decl.name.clone(),
            VarEntry {
                unique_name: unique_name.clone(),
                from_current_block: true,
            },
        );

        let init = decl
            .init
            .is_some()
            .then(|| resolve_expr(decl.init.as_ref().unwrap(), var_map));

        Decl {
            name: unique_name,
            init,
        }
    }
}

fn resolve_stmt(stmt: &Stmt, var_map: &VarMap) -> Stmt {
    match stmt {
        Stmt::Return { expr } => Stmt::Return {
            expr: resolve_expr(expr, var_map),
        },
        Stmt::Expression { expr } => Stmt::Expression {
            expr: resolve_expr(expr, var_map),
        },
        Stmt::If {
            condition,
            then,
            otherwise,
        } => Stmt::If {
            condition: resolve_expr(condition, var_map),
            then: Box::from(resolve_stmt(then, var_map)),
            otherwise: if let Some(otherwise) = otherwise {
                Some(Box::from(resolve_stmt(otherwise, var_map)))
            } else {
                None
            },
        },
        Stmt::Null => Stmt::Null,
        Stmt::Goto { label } => Stmt::Goto {
            label: label.clone(),
        },
        Stmt::LabeledStmt { label, stmt } => Stmt::LabeledStmt {
            label: label.clone(),
            stmt: Box::from(resolve_stmt(stmt, var_map)),
        },

        Stmt::Compound { block } => {
            let mut var_map = copy_var_map(var_map);
            Stmt::Compound {
                block: resolve_block(block, &mut var_map)
            }
        }
    }
}

fn resolve_expr(expr: &Expr, var_map: &VarMap) -> Expr {
    match expr {
        Expr::Constant(c) => Expr::Constant(*c),
        Expr::Var(name) => {
            if var_map.contains_key(name) {
                return Expr::Var(var_map.get(name).unwrap().unique_name.clone());
            } else {
                panic!("Undeclared Variable")
            }
        }
        Expr::Unary { op, expr } => match op {
            UnaryOp::Inc | UnaryOp::Dec => {
                if !matches!(**expr, Expr::Var(_)) {
                    panic!("Operand of ++/-- must be variable");
                }

                Expr::Unary {
                    op: *op,
                    expr: Box::from(resolve_expr(expr, var_map)),
                }
            }
            _ => Expr::Unary {
                op: *op,
                expr: Box::from(resolve_expr(expr, var_map)),
            },
        },
        Expr::Binary { op, left, right } => Expr::Binary {
            op: op.clone(),
            left: Box::from(resolve_expr(left, var_map)),
            right: Box::from(resolve_expr(right, var_map)),
        },
        Expr::Assignment { lvalue, expr } => {
            if !matches!(**lvalue, Expr::Var(_)) {
                panic!("Invalid lvalue");
            }
            Expr::Assignment {
                lvalue: Box::from(resolve_expr(lvalue, var_map)),
                expr: Box::from(resolve_expr(expr, var_map)),
            }
        }
        Expr::Conditional {
            condition,
            then,
            otherwise,
        } => Expr::Conditional {
            condition: Box::new(resolve_expr(condition, var_map)),
            then: Box::new(resolve_expr(then, var_map)),
            otherwise: Box::new(resolve_expr(otherwise, var_map)),
        },
        Expr::CompoundAssignment { op, lvalue, expr } => {
            if !matches!(**lvalue, Expr::Var(_)) {
                panic!("Invalid lvalue");
            }
            Expr::CompoundAssignment {
                op: *op,
                lvalue: Box::from(resolve_expr(lvalue, var_map)),
                expr: Box::from(resolve_expr(expr, var_map)),
            }
        }
        Expr::PostfixInc(expr) => {
            if !matches!(**expr, Expr::Var(_)) {
                panic!("Invalid lvalue");
            }

            Expr::PostfixInc(Box::from(resolve_expr(expr, var_map)))
        }
        Expr::PostfixDec(expr) => {
            if !matches!(**expr, Expr::Var(_)) {
                panic!("Invalid lvalue");
            }

            Expr::PostfixDec(Box::from(resolve_expr(expr, var_map)))
        }
    }
}

fn copy_var_map(var_map: &VarMap) -> VarMap {
    let new_map = var_map.iter().map(|(key, entry)| {
        (key.clone(), VarEntry {
            unique_name: entry.unique_name.clone(),
            from_current_block: false,
        })
    }).collect();

    new_map
}
