use std::collections::HashMap;

use ast::*;
use unique_ident::make_label;

use crate::sem_err::*;

type CaseMap = HashMap<Option<i32>, String>;

pub fn analyze_switches(program: &mut TranslationUnit) -> SemanticResult<()> {
    Ok(analyze_func(&mut program.func))?
}

fn analyze_func(func: &mut Func) -> SemanticResult<()> {
    Ok(analyze_block(&mut func.body, &mut None)?)
}

fn analyze_block(block: &mut Block, case_map: &mut Option<CaseMap>) -> SemanticResult<()> {
    for item in &mut block.items {
        analyze_block_item(item, case_map)?
    }

    Ok(())
}

fn analyze_block_item(
    block_item: &mut BlockItem,
    case_map: &mut Option<CaseMap>,
) -> SemanticResult<()> {
    match block_item {
        BlockItem::S(stmt) => analyze_stmt(stmt, case_map)?,
        BlockItem::D(_) => {}
    }

    Ok(())
}

fn analyze_stmt(stmt: &mut Stmt, case_map: &mut Option<CaseMap>) -> SemanticResult<()> {
    match stmt {
        Stmt::Default { body, label } => analyze_case(None, case_map, "default", body, label),
        Stmt::Case {
            constant,
            body,
            label,
        } => {
            let constant = match constant {
                Expr::Constant(c) => Some(*c),
                _ => {
                    return Err(SemErr::new(format!(
                        "Non-constant label in case statement: {:#?}",
                        constant
                    )))
                }
            };

            analyze_case(constant, case_map, "case", body, label)
        }
        Stmt::Switch {
            control: _,
            body,
            label: _,
        } => {
            let case_map = CaseMap::new();

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
    key: Option<i32>,
    case_map: &mut Option<CaseMap>,
    label: &str,
    body: &mut Stmt,
    case_label: &mut String
) -> SemanticResult<()> {
    // Check if in switch
    case_map
        .as_mut()
        .expect("Found case statement outside of switch");

    // check for duplicate cases
    if case_map.as_mut().unwrap().contains_key(&key) {
        let error = match key {
            Some(i) => SemErr::new(format!("Duplicate case found in switch statement: {}", i)),
            None => SemErr::new(format!("Duplicate default found in switch statement")),
        };

        return Err(error);
    }

    // generate new label - "case" or "default"
    *case_label = make_label(label);
    case_map.as_mut().unwrap().insert(key, case_label.clone());

    analyze_stmt(body, case_map)?;

    Ok(())
}
