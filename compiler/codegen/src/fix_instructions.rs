use lir::*;
use ty::{Scope, Symbol};

pub fn fix_invalid_instructions(program: &mut Program, symbols: &mut Scope<Symbol>) -> Program {
    Program {
        funcs: program
            .funcs
            .iter()
            .map(|func| fix_func(func, symbols))
            .collect(),
    }
}

fn fix_func(func: &Func, symbols: &mut Scope<Symbol>) -> Func {
    let stack_bytes = symbols.get(&func.name).unwrap().stack_frame_size;

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
    }
}

fn fix_instructions(instructions: &Vec<Instruction>) -> Vec<Instruction> {
    let mut fixed_instr = vec![];

    for i in instructions {
        match i {
            Instruction::Mov {
                src: Operand::Stack(src),
                dest: Operand::Stack(dest),
            } => {
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Stack(*src),
                    dest: Operand::Register(Register::R10),
                });
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Register(Register::R10),
                    dest: Operand::Stack(*dest),
                });
            }
            Instruction::Binary {
                op:
                    op @ BinaryOp::Add
                    | op @ BinaryOp::Sub
                    | op @ BinaryOp::And
                    | op @ BinaryOp::Or
                    | op @ BinaryOp::Xor,
                src: Operand::Stack(src),
                dest: Operand::Stack(dest),
            } => {
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Stack(*src),
                    dest: Operand::Register(Register::R10),
                });
                fixed_instr.push(Instruction::Binary {
                    op: op.clone(),
                    src: Operand::Register(Register::R10),
                    dest: Operand::Stack(*dest),
                });
            }
            Instruction::Binary {
                op: BinaryOp::Mult,
                src,
                dest: Operand::Stack(dest),
            } => {
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Stack(*dest),
                    dest: Operand::Register(Register::R11),
                });
                fixed_instr.push(Instruction::Binary {
                    op: BinaryOp::Mult,
                    src: src.clone(),
                    dest: Operand::Register(Register::R11),
                });
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Register(Register::R11),
                    dest: Operand::Stack(*dest),
                });
            }
            Instruction::Idiv(Operand::Imm(val)) => {
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Imm(*val),
                    dest: Operand::Register(Register::R10),
                });
                fixed_instr.push(Instruction::Idiv(Operand::Register(Register::R10)));
            }
            Instruction::Cmp(Operand::Stack(first), Operand::Stack(second)) => {
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Stack(*first),
                    dest: Operand::Register(Register::R10),
                });
                fixed_instr.push(Instruction::Cmp(
                    Operand::Register(Register::R10),
                    Operand::Stack(*second),
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
