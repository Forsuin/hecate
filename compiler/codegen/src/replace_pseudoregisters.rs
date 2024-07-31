use std::collections::HashMap;

use lir::*;

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

    fn replace_operand(&mut self, operand: &Operand) -> Operand {
        match operand {
            Operand::Pseudo(var) => {
                match self.offset_map.get(var) {
                    None => {
                        let new_offset = self.current_offset + 4;
                        self.current_offset = new_offset;
                        self.offset_map.insert(var.clone(), new_offset);
                        Operand::Stack(new_offset)
                    }
                    Some(offset) => {
                        Operand::Stack(*offset)
                    }
                }
            }
            _ => operand.clone()
        }
    }

    fn replace_instruction(&mut self, instruction: &Instruction) -> Instruction {
        match instruction {
            Instruction::Mov {src, dest} => {
                let src = self.replace_operand(src);
                let dest = self.replace_operand(dest);
                Instruction::Mov {src, dest}
            }
            Instruction::Unary { op, dest } => {
                let dest = self.replace_operand(dest);
                Instruction::Unary {
                    op: op.clone(),
                    dest,
                }
            }
            Instruction::AllocateStack(_) => {
                panic!("AllocateStack should not be present");
            }
            Instruction::Ret => {
                Instruction::Ret
            }
        }
    }

    fn replace_func(&mut self, func: &Function) -> Function {
        let mut fixed_instructions = vec![];

        for i in &func.instructions {
            fixed_instructions.push(self.replace_instruction(i));
        }

        Function {
            name: func.name.clone(),
            instructions: fixed_instructions,
        }
    }

    pub fn replace_psuedos(assm_ast: &Program) -> (Program, i32) {
        let mut state = PseudoReplacer::new();

        (
            Program {
                func: state.replace_func(&assm_ast.func),
            },
            state.current_offset,
        )
    }
}
