use std::collections::HashSet;

use ast::*;

use crate::sem_err::SemErr;

pub fn validate_labels(program: &TranslationUnit) -> Result<(), SemErr> {
    Ok(validate_func(&program.func)?)
}

type LabelSet = HashSet<String>;

fn validate_func(func: &Func) -> Result<(), SemErr>  {
    let mut defined = HashSet::new();
    let mut used = HashSet::new();

    validate_block(&func.body, &mut defined, &mut used)?;

    let undefined: Vec<_> = used.difference(&defined).map(String::as_str).collect();

    if !undefined.is_empty() {
        return Err(SemErr::new(format!("Found labels that are used but not defined: {:?}", undefined.join(", "))));
    }

    Ok(())
}

fn validate_block(block: &Block, defined: &mut LabelSet, used: &mut LabelSet)-> Result<(), SemErr> {
    for item in &block.items {
        validate_block_item(item, defined, used)?;
    }

    Ok(())
}

fn validate_block_item(item: &BlockItem, defined: &mut LabelSet, used: &mut LabelSet)-> Result<(), SemErr> {
    match item {
        BlockItem::S(stmt) => validate_stmt(stmt, defined, used),
        BlockItem::D(_) => {
            Ok(())
        }
    }
}

fn validate_stmt(stmt: &Stmt, defined: &mut LabelSet, used: &mut LabelSet)-> Result<(), SemErr> {
    match stmt {
        Stmt::Goto { label } => {
            used.insert(label.clone());
            Ok(())
        }
        Stmt::LabeledStmt { label, stmt } => {
            if defined.contains(label) {
                return Err(SemErr::new(format!("Duplicate label: {}", label)));
            }

            defined.insert(label.clone());
            Ok(validate_stmt(stmt, defined, used)?)
        }
        Stmt::If { condition: _condition, then, otherwise } => {
            validate_stmt(then, defined, used)?;
            if let Some(otherwise) = otherwise {
                validate_stmt(otherwise, defined, used)?;
            }

            Ok(())
        }
        Stmt::Compound { block } => {
            for item in &block.items {
                validate_block_item(item, defined, used)?;
            }

            Ok(())
        }
        Stmt::While { condition: _, body, label: _ } => {
            validate_stmt(body, defined, used)?;
            Ok(())
        }
        Stmt::DoWhile { body, condition: _, label: _ } => {
            validate_stmt(body, defined, used)?;
            Ok(())
        }
        Stmt::For { init: _, condition: _, post: _, body, label: _ } => {
            validate_stmt(body, defined, used)?;
            Ok(())
        }
        Stmt::Return { .. } | Stmt::Null | Stmt::Expression { .. } | Stmt::Break { .. } | Stmt::Continue { .. } => { Ok(()) }
    }
}