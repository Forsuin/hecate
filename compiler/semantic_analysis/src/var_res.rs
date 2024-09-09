use std::collections::HashMap;

use ast::*;
use unique_ident::make_temp_name;
use crate::sem_err::SemErr;
// var's original name -> (unique_name, from_current_block)

struct VarEntry {
    pub unique_name: String,
    pub from_current_block: bool,
}

type VarMap = HashMap<String, VarEntry>;

pub fn resolve(program: &mut TranslationUnit) -> Result<(), SemErr> {
    resolve_func(&mut program.func)?;

    Ok(())
}

fn resolve_func(func: &mut Func) -> Result<(), SemErr> {
    let mut var_map = VarMap::new();

    resolve_block(&mut func.body, &mut var_map)?;

    Ok(())
}

fn resolve_block(block: &mut Block, var_map: &mut VarMap)-> Result<(), SemErr> {

    for block_item in &mut block.items {
        resolve_block_item(block_item, var_map)?
    }

    Ok(())
}

fn resolve_block_item(item: &mut BlockItem, var_map: &mut VarMap) -> Result<(), SemErr> {
    match item {
        BlockItem::S(stmt) => resolve_stmt(stmt, var_map)?,
        BlockItem::D(decl) => resolve_decl(decl, var_map)?,
    }

    Ok(())
}

fn resolve_decl(decl: &mut Decl, var_map: &mut VarMap) -> Result<(), SemErr> {
    if var_map.contains_key(&decl.name) && var_map.get(&decl.name).unwrap().from_current_block {
        return Err(SemErr::new(format!("Duplicate variable declaration found: {:#?}", decl.name)));
    } else {
        let unique_name = make_temp_name(&decl.name);
        var_map.insert(
            decl.name.clone(),
            VarEntry {
                unique_name: unique_name.clone(),
                from_current_block: true,
            },
        );

        if let Some(init) = decl.init.as_mut() {
            resolve_expr(init, var_map)?;
        }

        decl.name = unique_name;

    }

    Ok(())
}

fn resolve_stmt(stmt: &mut Stmt, var_map: &VarMap) -> Result<(), SemErr> {
    match stmt {
        Stmt::Return { expr } => {
            resolve_expr(expr, var_map)?;
        },
        Stmt::Expression { expr } => {
            resolve_expr(expr, var_map)?;
        },
        Stmt::If {
            condition,
            then,
            otherwise,
        } => {
             resolve_expr(condition, var_map)?;
             resolve_stmt(then, var_map)?;
             if let Some(otherwise) = otherwise {
                resolve_stmt(otherwise, var_map)?;
            }
        },
        Stmt::Null => {},
        Stmt::Goto { label: _ } => {},
        Stmt::LabeledStmt { label: _, stmt } =>  {
            resolve_stmt(stmt, var_map)?;
        },
        Stmt::Compound { block } => {
            let mut new_map = copy_var_map(var_map);
            resolve_block(block, &mut new_map)?;
        }
        Stmt::Break { .. } => {}
        Stmt::Continue { .. } => {}
        Stmt::While { condition, body, label: _ } => {
            resolve_expr(condition, var_map)?;
            resolve_stmt(body, var_map)?;
        }
        Stmt::DoWhile { body, condition, label: _ } => {
            resolve_stmt(body, var_map)?;
            resolve_expr(condition, var_map)?;
        }
        Stmt::For { init, condition, post, body, label: _ } => {
            let mut var_map = copy_var_map(var_map);

            resolve_for_init(init, &mut var_map)?;
            resolve_optional_expr(condition, &mut var_map)?;
            resolve_optional_expr(post, &mut  var_map)?;
            resolve_stmt(body, &mut var_map)?;
        }
    }

    Ok(())
}

fn resolve_for_init(init: &mut ForInit, var_map: &mut VarMap) -> Result<(), SemErr> {
    match init {
        ForInit::Decl(decl) => resolve_decl(decl, var_map)?,
        ForInit::Expr(expr) => resolve_optional_expr(expr, var_map)?
    }

    Ok(())
}

fn resolve_optional_expr(expr: &mut Option<Expr>, var_map: &VarMap) -> Result<(), SemErr> {
    match expr {
        Some(expr) => {
            resolve_expr(expr, var_map)?
        },
        None => {}
    }

    Ok(())
}

fn resolve_expr(expr: &mut Expr, var_map: &VarMap) -> Result<(), SemErr> {
    match expr {
        Expr::Constant(_) => {},
        Expr::Var(name) => {
            if var_map.contains_key(name) {
                *name = var_map.get(&name.clone()).unwrap().unique_name.clone();
            }
            else {
                return Err(SemErr::new(format!("Undeclared Variable")));
            }
        }
        Expr::Unary { op, expr } => {
            if matches!(op, UnaryOp::Inc | UnaryOp::Dec) {
                if !matches!(**expr, Expr::Var(_)) {
                    return Err(SemErr::new(format!("Operand of ++/-- must be variable")));
                }
            }
            resolve_expr(expr, var_map)?;
        },
        Expr::Binary { op: _, left, right } => {
            resolve_expr(left, var_map)?;
            resolve_expr(right, var_map)?;
        },
        Expr::Assignment { lvalue, expr } => {
            if !matches!(**lvalue, Expr::Var(_)) {
                return Err(SemErr::new(format!("Invalid l-value")));
            }

            resolve_expr(lvalue, var_map)?;
            resolve_expr(expr, var_map)?;
        }
        Expr::Conditional {
            condition,
            then,
            otherwise,
        } => {
            resolve_expr(condition, var_map)?;
            resolve_expr(then, var_map)?;
            resolve_expr(otherwise, var_map)?;
        },
        Expr::CompoundAssignment { op: _, lvalue, expr } => {
            if !matches!(**lvalue, Expr::Var(_)) {
                return Err(SemErr::new(format!("Invalid l-value")));
            }

            resolve_expr(lvalue, var_map)?;
            resolve_expr(expr, var_map)?;
        }
        Expr::PostfixInc(expr) => {
            if !matches!(**expr, Expr::Var(_)) {
                return Err(SemErr::new(format!("Invalid l-value")));
            }

            resolve_expr(expr, var_map)?;
        }
        Expr::PostfixDec(expr) => {
            if !matches!(**expr, Expr::Var(_)) {
                return Err(SemErr::new(format!("Invalid l-value")));
            }
            resolve_expr(expr, var_map)?;
        }
    }

    Ok(())
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
