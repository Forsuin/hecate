use lir::*;
use ty::SymbolTable;

pub fn fix_invalid_instructions(program: &mut Program, symbols: &mut SymbolTable) -> Program {
    Program {
        decls: program
            .decls
            .iter()
            .map(|decl| fix_decl(decl, symbols))
            .collect(),
    }
}

fn fix_decl(decl: &Decl, symbols: &mut SymbolTable) -> Decl {
    match decl {
        Decl::Func(func) => {
            Decl::Func(fix_func(func, symbols))
        }
        Decl::StaticVar(_) => { decl.clone() }
    }
}

fn fix_func(func: &Func, symbols: &mut SymbolTable) -> Func {
    let stack_bytes = symbols.get_bytes_required(&func.name).unwrap();


    let round_away_from_zero = |n: i32, x: i32| {
        if x % n == 0 {
            x
        } else if x < 0 {
            x - n - (x % n)
        } else {
            x + n - (x & n)
        }
    };

    let mut instructions = vec![Instruction::AllocateStack(-round_away_from_zero(
        16,
        stack_bytes,
    ))];

    instructions.append(&mut fix_instructions(&func.instructions));

    Func {
        name: func.name.clone(),
        instructions,
        global: func.global,
    }
}

fn fix_instructions(instructions: &Vec<Instruction>) -> Vec<Instruction> {
    let mut fixed_instr = vec![];

    for i in instructions {
        match i {
            Instruction::Mov {
                src: src@ Operand::Stack(_) | src @ Operand::Data(_),
                dest: dest @ Operand::Stack(_) | dest@ Operand::Data(_),
            } => {
                fixed_instr.push(Instruction::Mov {
                    src: src.clone(),
                    dest: Operand::Register(Register::R10),
                });
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Register(Register::R10),
                    dest: dest.clone(),
                });
            }
            Instruction::Binary {
                op:
                    op @ BinaryOp::Add
                    | op @ BinaryOp::Sub
                    | op @ BinaryOp::And
                    | op @ BinaryOp::Or
                    | op @ BinaryOp::Xor,
                src: src @ Operand::Stack(_) | src @ Operand::Data(_),
                dest: dest @ Operand::Stack(_) | dest @ Operand::Data(_),
            } => {
                fixed_instr.push(Instruction::Mov {
                    src: src.clone(),
                    dest: Operand::Register(Register::R10),
                });
                fixed_instr.push(Instruction::Binary {
                    op: op.clone(),
                    src: Operand::Register(Register::R10),
                    dest: dest.clone(),
                });
            }
            Instruction::Binary {
                op: BinaryOp::Mult,
                src,
                dest: dest @ Operand::Stack(_) | dest @ Operand::Data(_),
            } => {
                fixed_instr.push(Instruction::Mov {
                    src: dest.clone(),
                    dest: Operand::Register(Register::R11),
                });
                fixed_instr.push(Instruction::Binary {
                    op: BinaryOp::Mult,
                    src: src.clone(),
                    dest: Operand::Register(Register::R11),
                });
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Register(Register::R11),
                    dest: dest.clone(),
                });
            }
            Instruction::Idiv(Operand::Imm(val)) => {
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Imm(*val),
                    dest: Operand::Register(Register::R10),
                });
                fixed_instr.push(Instruction::Idiv(Operand::Register(Register::R10)));
            }
            Instruction::Cmp(first @ Operand::Stack(_) | first @ Operand::Data(_), second @ Operand::Stack(_) | second @ Operand::Data(_)) => {
                fixed_instr.push(Instruction::Mov {
                    src: first.clone(),
                    dest: Operand::Register(Register::R10),
                });
                fixed_instr.push(Instruction::Cmp(
                    Operand::Register(Register::R10),
                    second.clone(),
                ));
            }
            Instruction::Cmp(first, Operand::Imm(val)) => {
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Imm(*val),
                    dest: Operand::Register(Register::R11),
                });
                fixed_instr.push(Instruction::Cmp(
                    first.clone(),
                    Operand::Register(Register::R11),
                ));
            }
            _ => fixed_instr.push(i.clone()),
        }
    }

    fixed_instr
}
