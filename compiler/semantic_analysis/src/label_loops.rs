use ast::*;
use unique_ident::make_label;

use crate::sem_err::SemErr;

pub fn label_loops(program: &mut TranslationUnit) -> Result<(), SemErr> {
    label_func(&mut program.func)?;
    Ok(())
}

fn label_func(func: &mut Func) -> Result<(), SemErr> {
    label_block(&mut func.body, None, None)?;
    Ok(())
}

fn label_block(block: &mut Block, cur_label: Option<String>, cur_continue_label: Option<String>) -> Result<(), SemErr> {
    for item in &mut block.items {
        label_item(item, cur_label.clone(), cur_continue_label.clone())?;
    }

    Ok(())
}

fn label_item(item: &mut BlockItem, cur_label: Option<String>, cur_continue_label: Option<String>) -> Result<(), SemErr> {
    match item {
        BlockItem::S(ref mut stmt) => label_stmt(stmt, cur_label, cur_continue_label)?,
        BlockItem::D(_) => {}
    }

    Ok(())
}

fn label_stmt(stmt: &mut Stmt, cur_break_label: Option<String>, cur_continue_label: Option<String>) -> Result<(), SemErr> {
    match stmt {
        Stmt::Break { label } => match cur_break_label {
            None => Err(SemErr::new(format!("Break outside of a loop"))),
            Some(l) => Ok(*label = l),
        },
        Stmt::Continue { label } => match cur_continue_label {
            None => Err(SemErr::new(format!("Continue outside of a loop"))),
            Some(l) => Ok(*label = l),
        },
        Stmt::While {
            condition: _,
            body,
            label,
        } => {
            *label = make_label("while");

            label_stmt(body, Some(label.clone()), Some(label.clone()))?;
            Ok(())
        }
        Stmt::DoWhile {
            body,
            condition: _,
            label,
        } => {
            *label = make_label("do_while");

            label_stmt(body, Some(label.clone()), Some(label.clone()))?;
            Ok(())
        }
        Stmt::For {
            init: _,
            condition: _,
            post: _,
            body,
            label,
        } => {
            *label = make_label("for");

            label_stmt(body, Some(label.clone()), Some(label.clone()))?;
            Ok(())
        }
        Stmt::Compound { block } => {
            label_block(block, cur_break_label, cur_continue_label)?;
            Ok(())
        }
        Stmt::If {
            condition: _,
            then,
            otherwise,
        } => {
            label_stmt(then, cur_break_label.clone(), cur_continue_label.clone())?;
            match otherwise {
                Some(otherwise) => {
                    label_stmt(otherwise, cur_break_label, cur_continue_label)?;
                }
                None => {}
            }
            Ok(())
        }
        Stmt::LabeledStmt { label: _, stmt } => {
            label_stmt(stmt, cur_break_label, cur_continue_label)?;
            Ok(())
        }

        Stmt::Switch { control: _, body, label } => {
            *label = make_label("switch");
            label_stmt(body, Some(label.clone()), cur_continue_label)?;
            Ok(())
        }
        Stmt::Case { constant: _, body, label: _ } => {
            label_stmt(body, cur_break_label, cur_continue_label)?;
            Ok(())
        }
        Stmt::Default { body, label: _ } => {
            label_stmt(body, cur_break_label, cur_continue_label)?;
            Ok(())
        }

        Stmt::Return { .. } | Stmt::Expression { .. } | Stmt::Goto { .. } | Stmt::Null => Ok(()),
    }
}
