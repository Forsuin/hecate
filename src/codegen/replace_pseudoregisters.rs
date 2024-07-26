use crate::{assembly};
use std::collections::HashMap;

#[derive(Debug)]
pub struct PseudoReplacer {
    // Offset from rbp, will be negative offset in emitted assembly
    current_offset: i32,
    offset_map: HashMap<String, i32>,
}

impl PseudoReplacer {
    fn new() -> Self {
        Self {
            current_offset: 0,
            offset_map: HashMap::new(),
        }
    }

    fn replace_operand(&mut self, operand: &assembly::Operand) -> assembly::Operand {
        match operand {
            assembly::Operand::Pseudo(var) => {
                match self.offset_map.get(var) {
                    None => {
                        let new_offset = self.current_offset + 4;
                        self.current_offset = new_offset;
                        self.offset_map.insert(var.clone(), new_offset);
                        assembly::Operand::Stack(new_offset)
                    }
                    Some(offset) => {
                        assembly::Operand::Stack(*offset)
                    }
                }
            }
            _ => operand.clone()
        }
    }

    fn replace_instruction(&mut self, instruction: &assembly::Instruction) -> assembly::Instruction {
        match instruction {
            assembly::Instruction::Mov {src, dest} => {
                let src = self.replace_operand(src);
                let dest = self.replace_operand(dest);
                assembly::Instruction::Mov {src, dest}
            }
            assembly::Instruction::Unary { op, dest } => {
                let dest = self.replace_operand(dest);
                assembly::Instruction::Unary {
                    op: op.clone(),
                    dest,
                }
            }
            assembly::Instruction::AllocateStack(_) => {
                panic!("AllocateStack should not be present");
            }
            assembly::Instruction::Ret => {
                assembly::Instruction::Ret
            }
        }
    }

    fn replace_func(&mut self, func: &assembly::Function) -> assembly::Function {
        let mut fixed_instructions = vec![];

        for i in &func.instructions {
            fixed_instructions.push(self.replace_instruction(i));
        }

        assembly::Function {
            name: func.name.clone(),
            instructions: fixed_instructions,
        }
    }

    pub fn replace_psuedos(assm_ast: &assembly::Program) -> (assembly::Program, i32) {
        let mut state = PseudoReplacer::new();

        (
            assembly::Program {
                func: state.replace_func(&assm_ast.func),
            },
            state.current_offset,
        )
    }
}
