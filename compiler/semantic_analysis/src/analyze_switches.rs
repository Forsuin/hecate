use std::collections::HashMap;

use ast::*;
use ty::{const_convert, SwitchableConstant, Type};
use unique_ident::make_label;

use crate::sem_err::*;

type CaseMap = HashMap<Option<SwitchableConstant>, String>;

#[derive(Clone)]
struct SwitchCtx {
    pub ty: Type,
    pub map: CaseMap,
}

impl SwitchCtx {
    pub fn new(ty: Type) -> Self {
        Self {
            ty,
            map: CaseMap::new(),
        }
    }
}

pub fn analyze_switches(program: &mut TranslationUnit) -> SemanticResult<()> {
    for decl in &mut program.decls {
        match decl {
            Decl::FuncDecl(func) => analyze_func(func)?,
            Decl::VarDecl(_) => {}
        }
    }

    Ok(())
}

fn analyze_func(func: &mut FuncDecl) -> SemanticResult<()> {
    if let Some(body) = &mut func.body {
        analyze_block(body, &mut None)?
    }

    Ok(())
}

fn analyze_block(block: &mut Block, case_map: &mut Option<SwitchCtx>) -> SemanticResult<()> {
    for item in &mut block.items {
        analyze_block_item(item, case_map)?
    }

    Ok(())
}

fn analyze_block_item(
    block_item: &mut BlockItem,
    case_map: &mut Option<SwitchCtx>,
) -> SemanticResult<()> {
    match block_item {
        BlockItem::S(stmt) => analyze_stmt(stmt, case_map)?,
        BlockItem::D(_) => {}
    }

    Ok(())
}

fn analyze_stmt(stmt: &mut Stmt, case_map: &mut Option<SwitchCtx>) -> SemanticResult<()> {
    match stmt {
        Stmt::Default { body, label } => analyze_case(None, case_map, "default", body, label),
        Stmt::Case {
            constant,
            body,
            label,
        } => {
            let const_expr = match &constant.kind {
                ExprKind::Constant(c) => Some(c.clone().into()),
                _ => {
                    return Err(SemErr::new(format!(
                        "Non-constant label in case statement: {:#?}",
                        constant
                    )))
                }
            };

            analyze_case(const_expr, case_map, "case", body, label)?;
            
            constant.set_type(case_map.clone().unwrap().ty);
            
            Ok(())
        }
        Stmt::Switch {
            control,
            body,
            label: _,
        } => {
            let switch_type = control.get_type().unwrap();

            let case_map = SwitchCtx::new(switch_type);

            analyze_stmt(body, &mut Some(case_map))
        }
        Stmt::If {
            condition: _,
            then,
            otherwise,
        } => {
            analyze_stmt(then, case_map)?;

            if let Some(otherwise) = otherwise {
                analyze_stmt(otherwise, case_map)?
            }

            Ok(())
        }
        Stmt::Compound { block } => analyze_block(block, case_map),
        Stmt::While {
            condition: _,
            body,
            label: _,
        } => analyze_stmt(body, case_map),
        Stmt::DoWhile {
            body,
            condition: _,
            label: _,
        } => analyze_stmt(body, case_map),
        Stmt::For {
            init: _,
            condition: _,
            post: _,
            body,
            label: _,
        } => analyze_stmt(body, case_map),
        Stmt::LabeledStmt { label: _, stmt } => analyze_stmt(stmt, case_map),

        Stmt::Return { .. }
        | Stmt::Null
        | Stmt::Expression { .. }
        | Stmt::Break { .. }
        | Stmt::Continue { .. }
        | Stmt::Goto { .. } => Ok(()),
    }
}

fn analyze_case(
    key: Option<SwitchableConstant>,
    case_map: &mut Option<SwitchCtx>,
    label: &str,
    body: &mut Stmt,
    case_label: &mut String,
) -> SemanticResult<()> {
    // Check if in switch
    case_map
        .as_mut()
        .expect("Found case statement outside of switch");

    // convert case to type of switch statement
    let key = key.map(|c| const_convert(&case_map.clone().unwrap().ty, c.clone().into()).into());

    // check for duplicate cases
    if case_map.as_mut().unwrap().map.contains_key(&key) {
        let error = match key {
            Some(i) => SemErr::new(format!("Duplicate case found in switch statement: {}", i)),
            None => SemErr::new("Duplicate default found in switch statement".to_string()),
        };

        return Err(error);
    }

    // generate new label - "case" or "default"
    *case_label = make_label(label);
    case_map.as_mut().unwrap().map.insert(key.clone(), case_label.clone());

    analyze_stmt(body, case_map)?;

    Ok(())
}
