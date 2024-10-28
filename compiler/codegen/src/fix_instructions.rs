use crate::round_away_from_zero;
use lir::symbol_table::AsmTable;
use lir::*;

pub fn fix_invalid_instructions(program: &mut Program, symbols: &mut AsmTable) -> Program {
    Program {
        decls: program
            .decls
            .iter()
            .map(|decl| fix_decl(decl, symbols))
            .collect(),
    }
}

fn fix_decl(decl: &Decl, symbols: &mut AsmTable) -> Decl {
    match decl {
        Decl::Func(func) => Decl::Func(fix_func(func, symbols)),
        Decl::StaticVar(_) => decl.clone(),
    }
}

fn fix_func(func: &Func, symbols: &mut AsmTable) -> Func {
    let stack_bytes = -symbols.get_bytes_required(&func.name);
    let mut instructions = vec![
        Instruction::Binary {
            op: BinaryOp::Sub,
            src: Operand::Imm(round_away_from_zero(
                16,
                stack_bytes,
            ) as i64),
            dest: Operand::Register(Register::SP),
            ty: AssemblyType::Quad,
        }
    ];

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
            // mov can't move a value from one memory address to another
            Instruction::Mov {
                src: src @ Operand::Stack(_) | src @ Operand::Data(_),
                dest: dest @ Operand::Stack(_) | dest @ Operand::Data(_),
                ty,
            } => {
                fixed_instr.push(Instruction::Mov {
                    src: src.clone(),
                    dest: Operand::Register(Register::R10),
                    ty: ty.clone(),
                });
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Register(Register::R10),
                    dest: dest.clone(),
                    ty: ty.clone(),
                });
            }
            // can't move a large constant to a memory address
            Instruction::Mov {
                src: src @ Operand::Imm(i),
                dest: dest @ Operand::Stack(_) | dest @ Operand::Data(_),
                ty: AssemblyType::Quad,
            } if is_large(*i) => {
                fixed_instr.push(Instruction::Mov {
                    src: src.clone(),
                    dest: Operand::Register(Register::R10),
                    ty: AssemblyType::Quad,
                });
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Register(Register::R10),
                    dest: dest.clone(),
                    ty: AssemblyType::Quad,
                })
            }
            // moving a quadword constant with a longword operand gives assembler warnings
            Instruction::Mov {
                src: Operand::Imm(i),
                dest,
                ty: AssemblyType::Long,
            } if is_larger_than_uint(*i) => {
                let reduced = *i & 0xffffffffi64;
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Imm(reduced),
                    dest: dest.clone(),
                    ty: AssemblyType::Long,
                })
            }
            // movsx can't have immediate source or memory dest
            Instruction::Movsx {
                src: src @ Operand::Imm(_),
                dest: dest @ Operand::Stack(_) | dest @ Operand::Data(_),
            } => fixed_instr.append(&mut vec![
                Instruction::Mov {
                    src: src.clone(),
                    dest: Operand::Register(Register::R10),
                    ty: AssemblyType::Long,
                },
                Instruction::Movsx {
                    src: Operand::Register(Register::R10),
                    dest: Operand::Register(Register::R11),
                },
                Instruction::Mov {
                    src: Operand::Register(Register::R11),
                    dest: dest.clone(),
                    ty: AssemblyType::Quad,
                },
            ]),
            Instruction::Movsx {
                src: src @ Operand::Imm(_),
                dest,
            } => fixed_instr.append(&mut vec![
                Instruction::Mov {
                    src: src.clone(),
                    dest: Operand::Register(Register::R10),
                    ty: AssemblyType::Long,
                },
                Instruction::Movsx {
                    src: Operand::Register(Register::R10),
                    dest: dest.clone(),
                },
            ]),
            Instruction::Movsx {
                src,
                dest: dest @ Operand::Stack(_) | dest @ Operand::Data(_),
            } => fixed_instr.append(&mut vec![
                Instruction::Movsx {
                    src: src.clone(),
                    dest: Operand::Register(Register::R11),
                },
                Instruction::Mov {
                    src: Operand::Register(Register::R11),
                    dest: dest.clone(),
                    ty: AssemblyType::Quad,
                },
            ]),
            // rewrite MovZeroExtend as either 1 or 2 Mov instructions
            Instruction::MovZeroExtend {
                src, dest
            } => {
                if let Operand::Register(_) = dest {
                    fixed_instr.push(Instruction::Mov {
                        src: src.clone(),
                        dest: dest.clone(),
                        ty: AssemblyType::Long,
                    })
                }
                else {
                    fixed_instr.append(&mut vec![
                        Instruction::Mov {
                            src: src.clone(),
                            dest: Operand::Register(Register::R11),
                            ty: AssemblyType::Long,
                        },
                        Instruction::Mov {
                            src: Operand::Register(Register::R11),
                            dest: dest.clone(),
                            ty: AssemblyType::Quad,
                        }
                    ])
                }
            }
            // Add/Sub/And/Or/Xor can't take large immediates as source operands
            Instruction::Binary {
                op:
                    op @ BinaryOp::Add
                    | op @ BinaryOp::Sub
                    | op @ BinaryOp::And
                    | op @ BinaryOp::Or
                    | op @ BinaryOp::Xor,
                src: src @ Operand::Imm(i),
                dest,
                ty: AssemblyType::Quad,
            } if is_large(*i) => fixed_instr.append(&mut vec![
                Instruction::Mov {
                    src: src.clone(),
                    dest: Operand::Register(Register::R10),
                    ty: AssemblyType::Quad,
                },
                Instruction::Binary {
                    op: op.clone(),
                    src: Operand::Register(Register::R10),
                    dest: dest.clone(),
                    ty: AssemblyType::Quad,
                },
            ]),
            // Add/Sub/And/Or/Xor can't have memory addresses for both operands
            Instruction::Binary {
                op:
                    op @ BinaryOp::Add
                    | op @ BinaryOp::Sub
                    | op @ BinaryOp::And
                    | op @ BinaryOp::Or
                    | op @ BinaryOp::Xor,
                src: src @ Operand::Stack(_) | src @ Operand::Data(_),
                dest: dest @ Operand::Stack(_) | dest @ Operand::Data(_),
                ty,
            } => {
                fixed_instr.push(Instruction::Mov {
                    src: src.clone(),
                    dest: Operand::Register(Register::R10),
                    ty: ty.clone(),
                });
                fixed_instr.push(Instruction::Binary {
                    op: op.clone(),
                    src: Operand::Register(Register::R10),
                    dest: dest.clone(),
                    ty: ty.clone(),
                });
            }
            // mult src can't be big operand and dest can't be in memory
            // both operands
            Instruction::Binary {
                op: BinaryOp::Mult,
                src: src @ Operand::Imm(i),
                dest: dest @ Operand::Stack(_) | dest @ Operand::Data(_),
                ty: AssemblyType::Quad,
            } if is_large(*i) => fixed_instr.append(&mut vec![
                Instruction::Mov {
                    src: src.clone(),
                    dest: Operand::Register(Register::R10),
                    ty: AssemblyType::Quad,
                },
                Instruction::Mov {
                    src: dest.clone(),
                    dest: Operand::Register(Register::R11),
                    ty: AssemblyType::Quad,
                },
                Instruction::Binary {
                    op: BinaryOp::Mult,
                    src: Operand::Register(Register::R10),
                    dest: Operand::Register(Register::R11),
                    ty: AssemblyType::Quad,
                },
                Instruction::Mov {
                    src: Operand::Register(Register::R11),
                    dest: dest.clone(),
                    ty: AssemblyType::Quad,
                },
            ]),
            // mult dest can't be in memory
            Instruction::Binary {
                op: BinaryOp::Mult,
                src,
                dest: dest @ Operand::Stack(_) | dest @ Operand::Data(_),
                ty,
            } => {
                fixed_instr.push(Instruction::Mov {
                    src: dest.clone(),
                    dest: Operand::Register(Register::R11),
                    ty: *ty,
                });
                fixed_instr.push(Instruction::Binary {
                    op: BinaryOp::Mult,
                    src: src.clone(),
                    dest: Operand::Register(Register::R11),
                    ty: *ty,
                });
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Register(Register::R11),
                    dest: dest.clone(),
                    ty: *ty,
                });
            }
            // mult src can't be big operand
            // just src
            Instruction::Binary {
                op: BinaryOp::Mult,
                src: src @ Operand::Imm(i),
                dest,
                ty: _,
            } if is_large(*i) => fixed_instr.append(&mut vec![
                Instruction::Mov {
                    src: src.clone(),
                    dest: Operand::Register(Register::R10),
                    ty: AssemblyType::Quad,
                },
                Instruction::Binary {
                    op: BinaryOp::Mult,
                    src: Operand::Register(Register::R10),
                    dest: dest.clone(),
                    ty: AssemblyType::Quad,
                },
            ]),
            // Idiv can't operate on constants
            Instruction::Idiv(Operand::Imm(val), ty) => {
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Imm(*val),
                    dest: Operand::Register(Register::R10),
                    ty: *ty,
                });
                fixed_instr.push(Instruction::Idiv(
                    Operand::Register(Register::R10),
                    *ty,
                ));
            }
            // Div can't operate on constants
            Instruction::Div(Operand::Imm(val), ty) => {
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Imm(*val),
                    dest: Operand::Register(Register::R10),
                    ty: *ty,
                });
                fixed_instr.push(Instruction::Div(
                    Operand::Register(Register::R10),
                    *ty,
                ));
            }
            // both cmp operands can't be in memory
            Instruction::Cmp(
                first @ Operand::Stack(_) | first @ Operand::Data(_),
                second @ Operand::Stack(_) | second @ Operand::Data(_),
                ty,
            ) => {
                fixed_instr.push(Instruction::Mov {
                    src: first.clone(),
                    dest: Operand::Register(Register::R10),
                    ty: ty.clone(),
                });
                fixed_instr.push(Instruction::Cmp(
                    Operand::Register(Register::R10),
                    second.clone(),
                    ty.clone(),
                ));
            }
            // first operand of cmp cannot be large constant
            Instruction::Cmp(src @ Operand::Imm(i), dest @ Operand::Imm(_), AssemblyType::Quad)
                if is_large(*i) =>
            {
                fixed_instr.append(&mut vec![
                    Instruction::Mov {
                        src: src.clone(),
                        dest: Operand::Register(Register::R10),
                        ty: AssemblyType::Quad,
                    },
                    Instruction::Mov {
                        src: dest.clone(),
                        dest: Operand::Register(Register::R11),
                        ty: AssemblyType::Quad,
                    },
                    Instruction::Cmp(
                        Operand::Register(Register::R10),
                        Operand::Register(Register::R11),
                        AssemblyType::Quad,
                    ),
                ])
            }
            Instruction::Cmp(src @ Operand::Imm(i), dest, AssemblyType::Quad) 
            if is_large(*i) => {
                fixed_instr.append(&mut vec![
                    Instruction::Mov {
                        src: src.clone(),
                        dest: Operand::Register(Register::R10),
                        ty: AssemblyType::Quad,
                    },
                    Instruction::Cmp(
                        Operand::Register(Register::R10),
                        dest.clone(),
                        AssemblyType::Quad,
                    ),
                ])
            }
            // second operand of cmp cannot be constant
            Instruction::Cmp(first, Operand::Imm(val), ty) => {
                fixed_instr.push(Instruction::Mov {
                    src: Operand::Imm(*val),
                    dest: Operand::Register(Register::R11),
                    ty: ty.clone(),
                });
                fixed_instr.push(Instruction::Cmp(
                    first.clone(),
                    Operand::Register(Register::R11),
                    ty.clone(),
                ));
            }
            Instruction::Push(src @ Operand::Imm(i)) if is_large(*i) => {
                fixed_instr.append(&mut vec![
                    Instruction::Mov {
                        src: src.clone(),
                        dest: Operand::Register(Register::R10),
                        ty: AssemblyType::Quad,
                    },
                    Instruction::Push(Operand::Register(Register::R10))
                ]);
            }
            _ => fixed_instr.push(i.clone()),
        }
    }

    fixed_instr
}

fn is_large(val: i64) -> bool {
    val > i32::MAX as i64 || val < i32::MIN as i64
}

fn is_larger_than_uint(val: i64) -> bool {
    let max_int = 2i64.pow(2) - 1;
    let min_int = i32::MIN as i64;
    val > max_int || val < min_int
}
