use std::collections::HashMap;

use lir::*;
use ty::SymbolTable;

#[derive(Debug)]
pub struct ReplacementState {
    // Offset from rbp, will be negative offset in emitted assembly
    current_offset: i32,
    offset_map: HashMap<String, i32>,
}

pub fn replace_psuedos(assm_ast: &Program, symbol_table: &mut SymbolTable) -> Program {
    Program {
        decls: assm_ast
            .decls
            .iter()
            .map(|decl| replace_decl(decl, symbol_table))
            .collect(),
    }
}

fn replace_decl(decl: &Decl, symbol_table: &mut SymbolTable) -> Decl {
    match decl {
        Decl::Func(func) => Decl::Func(replace_func(func, symbol_table)),
        Decl::StaticVar(_) => decl.clone(),
    }
}

fn replace_func(func: &Func, symbol_table: &mut SymbolTable) -> Func {
    let mut state = ReplacementState {
        current_offset: 0,
        offset_map: HashMap::new(),
    };
    let fixed_instructions = func
        .instructions
        .iter()
        .map(|instr| replace_instruction(instr, &mut state, symbol_table))
        .collect();

    symbol_table
        .set_bytes_required(&func.name, state.current_offset)
        .unwrap();

    Func {
        name: func.name.clone(),
        instructions: fixed_instructions,
        global: func.global,
    }
}

fn replace_instruction(
    instruction: &Instruction,
    state: &mut ReplacementState,
    symbol_table: &mut SymbolTable,
) -> Instruction {
    match instruction {
        Instruction::Mov { src, dest } => {
            let src = replace_operand(src, state, symbol_table);
            let dest = replace_operand(dest, state, symbol_table);
            Instruction::Mov { src, dest }
        }
        Instruction::Unary { op, dest } => {
            let dest = replace_operand(dest, state, symbol_table);
            Instruction::Unary {
                op: op.clone(),
                dest,
            }
        }
        Instruction::AllocateStack(amt) => Instruction::AllocateStack(*amt),
        Instruction::Ret => Instruction::Ret,
        Instruction::Binary { op, src, dest } => {
            let src = replace_operand(src, state, symbol_table);
            let dest = replace_operand(dest, state, symbol_table);
            Instruction::Binary {
                op: op.clone(),
                src,
                dest,
            }
        }
        Instruction::Idiv(op) => {
            let op = replace_operand(op, state, symbol_table);
            Instruction::Idiv(op)
        }
        Instruction::Cdq => Instruction::Cdq,
        Instruction::Cmp(first, second) => {
            let first = replace_operand(first, state, symbol_table);
            let second = replace_operand(second, state, symbol_table);
            Instruction::Cmp(first, second)
        }
        Instruction::Jmp { label } => Instruction::Jmp {
            label: label.clone(),
        },
        Instruction::JmpCond { condition, label } => Instruction::JmpCond {
            condition: condition.clone(),
            label: label.clone(),
        },
        Instruction::SetCond { condition, dest } => {
            let dest = replace_operand(dest, state, symbol_table);
            Instruction::SetCond {
                condition: condition.clone(),
                dest,
            }
        }
        Instruction::Label(ident) => Instruction::Label(ident.clone()),
        Instruction::DeallocateStack(amt) => Instruction::DeallocateStack(*amt),
        Instruction::Push(op) => Instruction::Push(replace_operand(op, state, symbol_table)),
        Instruction::Call(fun) => Instruction::Call(fun.clone()),
    }
}

fn replace_operand(
    operand: &Operand,
    state: &mut ReplacementState,
    symbols: &mut SymbolTable,
) -> Operand {
    match operand {
        Operand::Pseudo(var) => {
            if symbols.is_static(var).unwrap() {
                Operand::Data(var.clone())
            } else {
                match state.offset_map.get(var) {
                    None => {
                        let new_offset = state.current_offset - 4;
                        state.current_offset = new_offset;
                        state.offset_map.insert(var.clone(), new_offset);
                        Operand::Stack(new_offset)
                    }
                    Some(offset) => Operand::Stack(*offset),
                }
            }
        }
        _ => operand.clone(),
    }
}
