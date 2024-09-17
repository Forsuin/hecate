use lir::*;
use mir::tacky;
use mir::tacky::Val;
use ty::{Scope, Symbol};
use crate::fix_instructions::fix_invalid_instructions;
use crate::replace_pseudoregisters::replace_psuedos;

mod fix_instructions;
mod replace_pseudoregisters;

macro_rules! tb {
    ($variant:ident) => {
        tacky::BinaryOp::$variant
    };
    ($head:ident | $($tail:ident)|+) => {
        tb!($head) | tb!($($tail)|+)
    };
}

const PARAM_PASSING_REGS: [Register; 6] = [
    Register::DI,
    Register::SI,
    Register::DX,
    Register::CX,
    Register::R8,
    Register::R9,
];

pub fn gen_assm(tacky: &tacky::TranslationUnit, symbol_table: &mut Scope<Symbol>) -> Program {
    let mut funcs = vec![];

    for func in &tacky.funcs {
        funcs.push(gen_func(func));
    }

    let prog = Program { funcs };

    let mut replaced = replace_psuedos(&prog, symbol_table);

    fix_invalid_instructions(&mut replaced, symbol_table)
}

fn gen_func(func: &tacky::Func) -> Func {
    let mut instructions = gen_params(&func.params);
    instructions.append(&mut gen_instructions(&func.instructions));

    Func {
        name: func.name.clone(),
        instructions,
    }
}

fn gen_params(params: &Vec<String>) -> Vec<lir::Instruction> {
    let try_split = params.split_at_checked(6);

    let (register_params, stack_params) = if let Some(split) = try_split {
        (split.0, split.1)
    } else {
        (params.as_slice(), &[] as &[String])
    };

    let mut instructions = vec![];

    // pass in registers
    for (i, param) in register_params.iter().enumerate() {
        let reg = &PARAM_PASSING_REGS[i];

        instructions.push(Instruction::Mov {
            src: Operand::Register(reg.clone()),
            dest: Operand::Pseudo(param.clone()),
        })
    }

    // pass on stack
    for (i, param) in stack_params.iter().enumerate() {
        instructions.push(Instruction::Mov {
            // stack has to grow up
            src: Operand::Stack(16 + (8 * i) as i32),
            dest: Operand::Pseudo(param.clone()),
        })
    }

    instructions
}

fn gen_instructions(instructions: &Vec<tacky::Instruction>) -> Vec<lir::Instruction> {
    let mut assm_instr = vec![];

    for i in instructions {
        match i {
            tacky::Instruction::Return(val) => {
                assm_instr.push(Instruction::Mov {
                    src: gen_operand(val),
                    dest: Operand::Register(Register::AX),
                });
                assm_instr.push(Instruction::Ret);
            }
            tacky::Instruction::Unary {
                op: tacky::UnaryOp::Not,
                src,
                dest,
            } => {
                assm_instr.push(Instruction::Cmp(Operand::Imm(0), gen_operand(src)));
                assm_instr.push(Instruction::Mov {
                    src: Operand::Imm(0),
                    dest: gen_operand(dest),
                });
                assm_instr.push(Instruction::SetCond {
                    condition: Condition::E,
                    dest: gen_operand(dest),
                });
            }
            tacky::Instruction::Unary { op, src, dest } => {
                assm_instr.push(Instruction::Mov {
                    src: gen_operand(src),
                    dest: gen_operand(dest),
                });
                assm_instr.push(Instruction::Unary {
                    op: gen_unary(op),
                    dest: gen_operand(dest),
                });
            }
            tacky::Instruction::Binary {
                op,
                first,
                second,
                dest,
            } => {
                if matches!(op, tacky::BinaryOp::Divide | tacky::BinaryOp::Modulo) {
                    assm_instr.push(Instruction::Mov {
                        src: gen_operand(first),
                        dest: Operand::Register(Register::AX),
                    });
                    assm_instr.push(Instruction::Cdq);
                    assm_instr.push(Instruction::Idiv(gen_operand(second)));
                    assm_instr.push(Instruction::Mov {
                        src: Operand::Register(if *op == tacky::BinaryOp::Divide {
                            Register::AX
                        } else {
                            Register::DX
                        }),
                        dest: gen_operand(dest),
                    })
                } else if matches!(
                    op,
                    tacky::BinaryOp::BitshiftLeft | tacky::BinaryOp::BitshiftRight
                ) {
                    match second {
                        Val::Constant(_) => {
                            assm_instr.push(Instruction::Mov {
                                src: gen_operand(first),
                                dest: gen_operand(dest),
                            });
                            assm_instr.push(Instruction::Binary {
                                op: gen_binary(op),
                                src: gen_operand(second),
                                dest: gen_operand(dest),
                            });
                        }
                        _ => {
                            assm_instr.push(Instruction::Mov {
                                src: gen_operand(first),
                                dest: gen_operand(dest),
                            });
                            assm_instr.push(Instruction::Mov {
                                src: gen_operand(second),
                                dest: Operand::Register(Register::CX),
                            });
                            assm_instr.push(Instruction::Binary {
                                op: gen_binary(op),
                                src: Operand::Register(Register::CX),
                                dest: gen_operand(dest),
                            });
                        }
                    }
                } else if matches!(
                    op,
                    tb!(Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual)
                ) {
                    assm_instr.push(Instruction::Cmp(gen_operand(second), gen_operand(first)));
                    assm_instr.push(Instruction::Mov {
                        src: Operand::Imm(0),
                        dest: gen_operand(dest),
                    });
                    assm_instr.push(Instruction::SetCond {
                        condition: gen_cond(op),
                        dest: gen_operand(dest),
                    });
                } else {
                    assm_instr.push(Instruction::Mov {
                        src: gen_operand(first),
                        dest: gen_operand(dest),
                    });
                    assm_instr.push(Instruction::Binary {
                        op: gen_binary(op),
                        src: gen_operand(second),
                        dest: gen_operand(dest),
                    })
                }
            }
            tacky::Instruction::Copy { src, dest } => assm_instr.push(Instruction::Mov {
                src: gen_operand(src),
                dest: gen_operand(dest),
            }),
            tacky::Instruction::Jump { target } => assm_instr.push(Instruction::Jmp {
                label: target.clone(),
            }),
            tacky::Instruction::JumpIfZero { condition, target } => {
                assm_instr.push(Instruction::Cmp(Operand::Imm(0), gen_operand(condition)));
                assm_instr.push(Instruction::JmpCond {
                    condition: Condition::E,
                    label: target.clone(),
                });
            }
            tacky::Instruction::JumpIfNotZero { condition, target } => {
                assm_instr.push(Instruction::Cmp(Operand::Imm(0), gen_operand(condition)));
                assm_instr.push(Instruction::JmpCond {
                    condition: Condition::NE,
                    label: target.clone(),
                });
            }
            tacky::Instruction::Label(identifier) => {
                assm_instr.push(Instruction::Label(identifier.clone()))
            }
            tacky::Instruction::FunCall {
                func_name,
                args,
                dest,
            } => {
                let try_split = args.split_at_checked(6);

                let (reg_params, stack_params) = if let Some(split) = try_split {
                    (split.0, split.1)
                } else {
                    (args.as_slice(), &[] as &[Val])
                };

                let stack_padding = if stack_params.len() % 2 == 0 { 0 } else { 8 };

                if stack_padding != 0 {
                    assm_instr.push(Instruction::AllocateStack(stack_padding));
                }

                for (i, param) in reg_params.iter().enumerate() {
                    let reg = &PARAM_PASSING_REGS[i];
                    let arg = gen_operand(param);

                    assm_instr.push(Instruction::Mov {
                        src: arg,
                        dest: Operand::Register(reg.clone()),
                    });
                }

                for param in stack_params.iter().rev() {
                    let arg = gen_operand(param);

                    if matches!(arg, Operand::Register(_) | Operand::Imm(_)) {
                        assm_instr.push(Instruction::Push(arg));
                    } else {
                        assm_instr.push(Instruction::Mov {
                            src: arg,
                            dest: Operand::Register(Register::AX),
                        });
                        assm_instr.push(Instruction::Push(Operand::Register(Register::AX)));
                    }
                }

                assm_instr.push(Instruction::Call(func_name.clone()));

                let bytes_to_remove = 8 * stack_params.len() as i32 + stack_padding;

                if bytes_to_remove != 0 {
                    assm_instr.push(Instruction::DeallocateStack(bytes_to_remove));
                }

                let dest = gen_operand(dest);
                assm_instr.push(Instruction::Mov {
                    src: Operand::Register(Register::AX),
                    dest,
                });
            }
        }
    }

    assm_instr
}

fn gen_unary(operator: &tacky::UnaryOp) -> UnaryOp {
    match operator {
        tacky::UnaryOp::Complement => UnaryOp::Not,
        tacky::UnaryOp::Negate => UnaryOp::Neg,
        // should never get here
        tacky::UnaryOp::Not => {
            panic!("Unable to directly convert TACKY 'Not' directly to assembly")
        }
    }
}

fn gen_binary(operator: &tacky::BinaryOp) -> BinaryOp {
    match operator {
        tacky::BinaryOp::Add => BinaryOp::Add,
        tacky::BinaryOp::Subtract => BinaryOp::Sub,
        tacky::BinaryOp::Multiply => BinaryOp::Mult,
        tacky::BinaryOp::BitwiseAnd => BinaryOp::And,
        tacky::BinaryOp::BitwiseOr => BinaryOp::Or,
        tacky::BinaryOp::BitwiseXor => BinaryOp::Xor,
        tacky::BinaryOp::BitshiftLeft => BinaryOp::Sal,
        tacky::BinaryOp::BitshiftRight => BinaryOp::Sar,
        _ => panic!("Unable to convert {:#?} into assembly BinaryOp", operator),
    }
}

fn gen_operand(operand: &tacky::Val) -> Operand {
    match operand {
        tacky::Val::Constant(val) => Operand::Imm(*val),
        tacky::Val::Var(var) => Operand::Pseudo(var.clone()),
    }
}

fn gen_cond(op: &tacky::BinaryOp) -> Condition {
    match op {
        tacky::BinaryOp::Equal => Condition::E,
        tacky::BinaryOp::NotEqual => Condition::NE,
        tacky::BinaryOp::Less => Condition::L,
        tacky::BinaryOp::LessEqual => Condition::LE,
        tacky::BinaryOp::Greater => Condition::G,
        tacky::BinaryOp::GreaterEqual => Condition::GE,
        _ => panic!("Internal Error: Not a condition operator: {:?}", op),
    }
}
