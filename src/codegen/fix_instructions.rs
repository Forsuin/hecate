use crate::assembly;

pub fn fix_invalid_instructions(ast: &mut assembly::Program, stack_size: i32) -> assembly::Program {
    let mut func = &mut ast.func;

    func.instructions.insert(0, assembly::Instruction::AllocateStack(stack_size));

    assembly::Program {
        func: fix_func(func),
    }

}

fn fix_func(func: &assembly::Function) -> assembly::Function {
    assembly::Function {
        name: func.name.clone(),
        instructions: fix_instructions(&func.instructions),
    }
}

fn fix_instructions(instructions: &Vec<assembly::Instruction>) -> Vec<assembly::Instruction> {
    let mut fixed_instr = vec![];

    for i in instructions {
        match i {
            assembly::Instruction::Mov {src, dest} => {
                fixed_instr.push(assembly::Instruction::Mov {src: src.clone(), dest: assembly::Operand::Register(assembly::Register::R10)});
                fixed_instr.push(assembly::Instruction::Mov {src: assembly::Operand::Register(assembly::Register::R10), dest: dest.clone()});
            },
            _ => {
                fixed_instr.push(i.clone())
            }
        }
    }

    fixed_instr
}