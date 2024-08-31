use std::collections::HashSet;
use ast::*;

pub fn validate_labels(program: &TranslationUnit) {
    validate_func(&program.func);
}

type LabelSet = HashSet<String>;

fn validate_func(func: &Func) {
    let mut defined = HashSet::new();
    let mut used = HashSet::new();

    for item in &func.body {
        validate_block_item(item, &mut defined, &mut used);
    }

    let undefined: Vec<_> = used.difference(&defined).map(String::as_str).collect();

    if !undefined.is_empty() {
        panic!("Found labels that are used but not defined: {:?}", undefined.join(", "));
    }
}

fn validate_block_item(item: &BlockItem, defined: &mut LabelSet, used: &mut LabelSet) {
    match item {
        BlockItem::S(stmt) => validate_stmt(stmt, defined, used),
        BlockItem::D(_) => {}
    }
}

fn validate_stmt(stmt: &Stmt, defined: &mut LabelSet, used: &mut LabelSet) {
    match stmt {
        Stmt::Goto { label } => {
            used.insert(label.clone());
        }
        Stmt::LabeledStmt { label, stmt } => {
            if defined.contains(label) {
               panic!("Duplicate label: {}", label);
            }

            defined.insert(label.clone());
            validate_stmt(stmt, defined, used);
        }
        Stmt::If { condition: _condition, then, otherwise } => {
            validate_stmt(then, defined, used);
            if let Some(otherwise) = otherwise {
                validate_stmt(otherwise, defined, used);
            }
        }
        _ => {}
    }
}