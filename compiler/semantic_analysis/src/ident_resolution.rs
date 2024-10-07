use std::collections::HashMap;

use ast::*;
use unique_ident::make_temp_name;

use crate::sem_err::{SemErr, SemanticResult};

// decls's original name -> (unique_name, from_current_scope, has_external_linkage)
struct IdentEntry {
    pub unique_name: String,
    pub from_current_scope: bool,
    pub has_linkage: bool,
}

type IdentMap = HashMap<String, IdentEntry>;

pub fn resolve(program: &mut TranslationUnit) -> SemanticResult<()> {
    let mut ident_map = IdentMap::new();

    for decl in &mut program.decls {
        match decl {
            Decl::FuncDecl(func) => resolve_func_decl(func, &mut ident_map)?,
            Decl::VarDecl(var) => resolve_file_scope_var(var, &mut ident_map)?,
        }
    }

    Ok(())
}

fn resolve_func_decl(func: &mut FuncDecl, ident_map: &mut IdentMap) -> SemanticResult<()> {
    if ident_map.contains_key(&func.ident) {
        let prev_entry = ident_map.get(&func.ident).unwrap();

        if prev_entry.from_current_scope && (!prev_entry.has_linkage) {
            return Err(SemErr::new(format!(
                "Duplicate function declaration: {}",
                func.ident
            )));
        }
    }

    ident_map.insert(
        func.ident.clone(),
        IdentEntry {
            unique_name: func.ident.clone(),
            from_current_scope: true,
            has_linkage: true,
        },
    );

    // copy ident_map since parameters can shadow names in outer scope
    let mut inner_map = copy_ident_map(ident_map);
    for param in &mut func.params {
        resolve_param(param, &mut inner_map)?;
    }

    if let Some(body) = &mut func.body {
        resolve_block(body, &mut inner_map)?;
    }

    Ok(())
}

fn resolve_file_scope_var(var: &mut VarDecl, ident_map: &mut IdentMap) -> SemanticResult<()> {
    ident_map.insert(
        var.name.clone(),
        IdentEntry {
            unique_name: var.name.clone(),
            from_current_scope: true,
            has_linkage: true,
        },
    );

    Ok(())
}

fn resolve_param(param: &mut String, ident_map: &mut IdentMap) -> SemanticResult<()> {
    resolve_local_var(param, &None, ident_map)
}

fn resolve_local_var(
    var: &mut String,
    storage_class: &Option<StorageClass>,
    ident_map: &mut IdentMap,
) -> SemanticResult<()> {
    match ident_map.get(var) {
        None => {}
        Some(prev_entry) => {
            if prev_entry.from_current_scope {
                if !(prev_entry.has_linkage && *storage_class == Some(StorageClass::Extern)) {
                    return Err(SemErr::new(format!(
                        "Conflicting local declarations for variable: '{}'",
                        var
                    )));
                }
            }
        }
    }

    if *storage_class == Some(StorageClass::Extern) {
        ident_map.insert(
            var.clone(),
            IdentEntry {
                unique_name: var.clone(),
                from_current_scope: true,
                has_linkage: true,
            },
        );
    } else {
        let unique_name = make_temp_name(var);
        ident_map.insert(
            var.clone(),
            IdentEntry {
                unique_name: unique_name.clone(),
                from_current_scope: true,
                has_linkage: false,
            },
        );

        *var = unique_name;
    }

    Ok(())
}

fn resolve_local_var_decl(var: &mut VarDecl, ident_map: &mut IdentMap) -> SemanticResult<()> {
    resolve_local_var(&mut var.name, &var.storage_class, ident_map)?;

    if let Some(init) = &mut var.init {
        resolve_expr(init, ident_map)?;
    }

    Ok(())
}

fn resolve_block(block: &mut Block, ident_map: &mut IdentMap) -> SemanticResult<()> {
    for block_item in &mut block.items {
        resolve_block_item(block_item, ident_map)?
    }

    Ok(())
}

fn resolve_block_item(item: &mut BlockItem, ident_map: &mut IdentMap) -> SemanticResult<()> {
    match item {
        BlockItem::S(stmt) => resolve_stmt(stmt, ident_map)?,
        BlockItem::D(decl) => resolve_decl(decl, ident_map)?,
    }

    Ok(())
}

fn resolve_decl(decl: &mut Decl, ident_map: &mut IdentMap) -> SemanticResult<()> {
    match decl {
        Decl::VarDecl(var) => resolve_local_var_decl(var, ident_map),
        Decl::FuncDecl(FuncDecl {
            ident,
            body: Some(_),
            ..
        }) => Err(SemErr::new(format!("Illegal nested function: {}", ident))),
        Decl::FuncDecl(FuncDecl {
            ident,
            storage_class: Some(StorageClass::Static),
            ..
        }) => Err(SemErr::new(format!(
            "static keyword not allowed on local function definitions: '{:?}'",
            ident
        ))),
        Decl::FuncDecl(func) => resolve_func_decl(func, ident_map),
    }
}

fn resolve_stmt(stmt: &mut Stmt, ident_map: &IdentMap) -> SemanticResult<()> {
    match stmt {
        Stmt::Return { expr } => {
            resolve_expr(expr, ident_map)?;
        }
        Stmt::Expression { expr } => {
            resolve_expr(expr, ident_map)?;
        }
        Stmt::If {
            condition,
            then,
            otherwise,
        } => {
            resolve_expr(condition, ident_map)?;
            resolve_stmt(then, ident_map)?;
            if let Some(otherwise) = otherwise {
                resolve_stmt(otherwise, ident_map)?;
            }
        }
        Stmt::Null => {}
        Stmt::Goto { label: _ } => {}
        Stmt::LabeledStmt { label: _, stmt } => {
            resolve_stmt(stmt, ident_map)?;
        }
        Stmt::Compound { block } => {
            let mut new_map = copy_ident_map(ident_map);
            resolve_block(block, &mut new_map)?;
        }
        Stmt::Break { .. } => {}
        Stmt::Continue { .. } => {}
        Stmt::While {
            condition,
            body,
            label: _,
        } => {
            resolve_expr(condition, ident_map)?;
            resolve_stmt(body, ident_map)?;
        }
        Stmt::DoWhile {
            body,
            condition,
            label: _,
        } => {
            resolve_stmt(body, ident_map)?;
            resolve_expr(condition, ident_map)?;
        }
        Stmt::For {
            init,
            condition,
            post,
            body,
            label: _,
        } => {
            let mut ident_map = copy_ident_map(ident_map);

            resolve_for_init(init, &mut ident_map)?;
            resolve_optional_expr(condition, &mut ident_map)?;
            resolve_optional_expr(post, &mut ident_map)?;
            resolve_stmt(body, &mut ident_map)?;
        }
        Stmt::Switch {
            control,
            body,
            label: _,
        } => {
            resolve_expr(control, ident_map)?;

            let mut ident_map = copy_ident_map(ident_map);

            resolve_stmt(body, &mut ident_map)?;
        }
        Stmt::Case {
            constant,
            body,
            label: _,
        } => {
            resolve_expr(constant, ident_map)?;
            resolve_stmt(body, ident_map)?;
        }
        Stmt::Default { body, label: _ } => {
            resolve_stmt(body, ident_map)?;
        }
    }

    Ok(())
}

fn resolve_for_init(init: &mut ForInit, ident_map: &mut IdentMap) -> SemanticResult<()> {
    match init {
        ForInit::Decl(decl) => resolve_local_var_decl(decl, ident_map)?,
        ForInit::Expr(expr) => resolve_optional_expr(expr, ident_map)?,
    }

    Ok(())
}

fn resolve_optional_expr(expr: &mut Option<Expr>, ident_map: &IdentMap) -> SemanticResult<()> {
    match expr {
        Some(expr) => resolve_expr(expr, ident_map)?,
        None => {}
    }

    Ok(())
}

fn resolve_expr(expr: &mut Expr, ident_map: &IdentMap) -> SemanticResult<()> {
    match &mut expr.kind {
        ExprKind::Constant(_) => {}
        ExprKind::Var(name) => {
            if ident_map.contains_key(name) {
                *name = ident_map.get(&name.clone()).unwrap().unique_name.clone();
            } else {
                return Err(SemErr::new(format!("Undeclared Variable: {}", name)));
            }
        }
        ExprKind::Unary { op, expr } => {
            if matches!(op, UnaryOp::Inc | UnaryOp::Dec) {
                if !matches!(expr.kind, ExprKind::Var(_)) {
                    return Err(SemErr::new(format!("Operand of ++/-- must be variable")));
                }
            }
            resolve_expr(expr, ident_map)?;
        }
        ExprKind::Binary { op: _, left, right } => {
            resolve_expr(left, ident_map)?;
            resolve_expr(right, ident_map)?;
        }
        ExprKind::Assignment { lvalue, expr } => {
            if !matches!(lvalue.kind, ExprKind::Var(_)) {
                return Err(SemErr::new(format!("Invalid l-value")));
            }

            resolve_expr(lvalue, ident_map)?;
            resolve_expr(expr, ident_map)?;
        }
        ExprKind::Conditional {
            condition,
            then,
            otherwise,
        } => {
            resolve_expr(condition, ident_map)?;
            resolve_expr(then, ident_map)?;
            resolve_expr(otherwise, ident_map)?;
        }
        ExprKind::CompoundAssignment {
            op: _,
            lvalue,
            expr,
        } => {
            if !matches!(lvalue.kind, ExprKind::Var(_)) {
                return Err(SemErr::new(format!("Invalid l-value")));
            }

            resolve_expr(lvalue, ident_map)?;
            resolve_expr(expr, ident_map)?;
        }
        ExprKind::PostfixInc(expr) => {
            if !matches!(expr.kind, ExprKind::Var(_)) {
                return Err(SemErr::new(format!("Invalid l-value")));
            }

            resolve_expr(expr, ident_map)?;
        }
        ExprKind::PostfixDec(expr) => {
            if !matches!(expr.kind, ExprKind::Var(_)) {
                return Err(SemErr::new(format!("Invalid l-value")));
            }
            resolve_expr(expr, ident_map)?;
        }
        ExprKind::FunctionCall { func, args } => {
            if ident_map.contains_key(func) {
                *func = ident_map.get(func).unwrap().unique_name.clone();

                for arg in args {
                    resolve_expr(arg, ident_map)?;
                }
            } else {
                return Err(SemErr::new(format!("Undeclared function: {}", func)));
            }
        }
        ExprKind::Cast { target_type: _, expr } => {
            resolve_expr(expr, ident_map)?;
        }
    }

    Ok(())
}

fn copy_ident_map(ident_map: &IdentMap) -> IdentMap {
    let new_map = ident_map
        .iter()
        .map(|(key, entry)| {
            (
                key.clone(),
                IdentEntry {
                    unique_name: entry.unique_name.clone(),
                    from_current_scope: false,

                    has_linkage: entry.has_linkage,
                },
            )
        })
        .collect();

    new_map
}
