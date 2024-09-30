use std::collections::HashSet;

use ast::*;

use crate::sem_err::*;

pub fn validate_labels(program: &mut TranslationUnit) -> SemanticResult<()> {
    for decl in &mut program.decls {
        match decl {
            Decl::FuncDecl(func) => {
                validate_func(func)?;
            }
            Decl::VarDecl(_) => {}
        }
    }

    Ok(())
}

type LabelSet = HashSet<String>;

fn validate_func(func: &mut FuncDecl) -> SemanticResult<()>  {
    let mut defined = HashSet::new();
    let mut used = HashSet::new();

    if let Some(body) = &mut func.body {
        validate_block(body, &mut defined, &mut used, &func.ident)?;
    }

    let undefined: Vec<_> = used.difference(&defined).map(String::as_str).collect();

    if !undefined.is_empty() {
        return Err(SemErr::new(format!("Found labels that are used but not defined: {:?}", undefined.join(", "))));
    }

    Ok(())
}

fn validate_block(block: &mut Block, defined: &mut LabelSet, used: &mut LabelSet, func_name: &str)-> SemanticResult<()> {
    for item in &mut block.items {
        validate_block_item(item, defined, used, func_name)?;
    }

    Ok(())
}

fn validate_block_item(item: &mut BlockItem, defined: &mut LabelSet, used: &mut LabelSet, func_name: &str)-> SemanticResult<()> {
    match item {
        BlockItem::S(stmt) => validate_stmt(stmt, defined, used, func_name),
        BlockItem::D(_) => {
            Ok(())
        }
    }
}

fn validate_stmt(stmt: &mut Stmt, defined: &mut LabelSet, used: &mut LabelSet, func_name: &str)-> SemanticResult<()> {
    match stmt {
        Stmt::Goto { label } => {

            *label = format!("{}.{}", func_name, label);

            used.insert(label.clone());
            Ok(())
        }
        Stmt::LabeledStmt { label, stmt } => {
            *label = format!("{}.{}", func_name, label);

            if defined.contains(label) {
                return Err(SemErr::new(format!("Duplicate label: {}", label)));
            }
            defined.insert(label.clone());
            Ok(validate_stmt(stmt, defined, used, func_name)?)
        }
        Stmt::If { condition: _condition, then, otherwise } => {
            validate_stmt(then, defined, used, func_name)?;
            if let Some(otherwise) = otherwise {
                validate_stmt(otherwise, defined, used, func_name)?;
            }

            Ok(())
        }
        Stmt::Compound { block } => {
            for item in &mut block.items {
                validate_block_item(item, defined, used, func_name)?;
            }

            Ok(())
        }
        Stmt::While { condition: _, body, label: _ } => {
            validate_stmt(body, defined, used, func_name)?;
            Ok(())
        }
        Stmt::DoWhile { body, condition: _, label: _ } => {
            validate_stmt(body, defined, used, func_name)?;
            Ok(())
        }
        Stmt::For { init: _, condition: _, post: _, body, label: _ } => {
            validate_stmt(body, defined, used, func_name)?;
            Ok(())
        }

        Stmt::Switch { control: _, body , label: _ } => {
            validate_stmt(body, defined, used, func_name)?;
            Ok(())
        }
        Stmt::Case { constant: _, body, label: _ } => {
            validate_stmt(body, defined, used, func_name)?;
            Ok(())
        }
        Stmt::Default { body, label: _ } => {
            validate_stmt(body, defined, used, func_name)?;
            Ok(())
        }

        Stmt::Return { .. } | Stmt::Null | Stmt::Expression { .. } | Stmt::Break { .. } | Stmt::Continue { .. } => { Ok(()) }
    }
}