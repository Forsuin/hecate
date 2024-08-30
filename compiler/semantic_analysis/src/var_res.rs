use std::collections::HashMap;

use ast::*;
use unique_ident::make_temp_name;

type VarMap = HashMap<String, String>;

pub fn resolve(program: &TranslationUnit) -> TranslationUnit {
    TranslationUnit { func: resolve_func(&program.func) }
}

fn resolve_func(func: &Func) -> Func {
    let mut var_map = VarMap::new();

    let mut resolved_body = vec![];

    for block_item in &func.body {
        resolved_body.push(resolve_block_item(block_item, &mut var_map))
    }

    Func { ident: func.ident.clone(), body: resolved_body }
}

fn resolve_block_item(item: &BlockItem, var_map: &mut VarMap) -> BlockItem {
    match item {
        BlockItem::S(stmt) => BlockItem::S(resolve_stmt(stmt, var_map)),
        BlockItem::D(decl) => BlockItem::D(resolve_decl(decl, var_map)),
    }
}

fn resolve_decl(decl: &Decl, var_map: &mut VarMap) -> Decl {
    if var_map.contains_key(&decl.name) {
        panic!("Duplicate variable declaration");
    }
    else {
        let unique_name = make_temp_name(&decl.name);
        var_map.insert(decl.name.clone(), unique_name.clone());

        let init = decl.init.is_some().then(|| resolve_expr(decl.init.as_ref().unwrap(), var_map));

        Decl { name: unique_name, init }
    }
}

fn resolve_stmt(stmt: &Stmt, var_map: &VarMap) -> Stmt {
    match stmt {
        Stmt::Return { expr } => Stmt::Return { expr: resolve_expr(expr, var_map) },
        Stmt::Expression { expr } => Stmt::Expression { expr: resolve_expr(expr, var_map) },
        Stmt::Null => Stmt::Null,
    }
}

fn resolve_expr(expr: &Expr, var_map: &VarMap) -> Expr {
    match expr {
        Expr::Constant(c) => Expr::Constant(*c),
        Expr::Var(name) => {
            if var_map.contains_key(name) {
                return Expr::Var(var_map.get(name).unwrap().to_string());
            } else {
                panic!("Undeclared Variable")
            }
        }
        Expr::Unary { op, expr } => Expr::Unary {
            op: *op,
            expr: Box::from(resolve_expr(expr, var_map)),
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
        },
        Expr::CompoundAssignment {op, lvalue, expr} => {
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
