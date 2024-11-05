use anyhow::{Ok, Result};
use clap::{Args, Parser as ClapParser};
use std::io::Write;
use std::{fs::read_to_string, path::Path, process::Command};
use thiserror::Error;

use codegen::gen_assm;
use emission::output;
use lexer::{Lexer, TokenType};
use mir::{debug_tacky, gen_tacky};
use parser::Parser;
use semantic_analysis::{analyze_switches, label_loops, resolve, validate_labels, TypeChecker};

#[derive(ClapParser, Debug)]
#[command(version, about, long_about = "Runs the Hecate C compiler")]
struct Arguments {
    /// Path to C source file
    path: String,

    /// "Specifies a point in compilation process for Hecate to stop, only one(1) option can be specified at a time"
    #[command(flatten)]
    stage_options: StageOptions,

    #[arg(short = 'l')]
    link_lib: Option<String>,
}

/// Run C compiler with optional arguments
#[derive(Args, Debug)]
#[group(required = false, multiple = true)]
struct StageOptions {
    /// Stop after lexer
    #[arg(long)]
    lex: bool,

    /// Stop after parser
    #[arg(long)]
    parse: bool,

    /// Stop after assembly generation
    #[arg(long)]
    codegen: bool,

    /// Stop after TACKY generation
    #[arg(long)]
    tacky: bool,

    /// Stop after semantic analysis
    #[arg(long)]
    validate: bool,

    /// Emit assembly file, but do not assemble or link it
    #[arg(short = 'S')]
    s: bool,

    /// Write out tacky code
    #[arg(short = 'd')]
    debug: bool,

    /// Compile and assemble into object file, but do not link
    #[arg(short = 'c')]
    c: bool,
}

/// Which stage the compiler should stop at
enum StopStage {
    Lexer,
    Parser,
    CodeGen,
    Assembler,
    Tacky,
    Analysis,
    Object,
}

impl StopStage {
    fn from_args(options: &StageOptions) -> Option<StopStage> {
        if options.lex {
            Some(StopStage::Lexer)
        } else if options.parse {
            Some(StopStage::Parser)
        } else if options.codegen {
            Some(StopStage::CodeGen)
        } else if options.s {
            Some(StopStage::Assembler)
        } else if options.tacky {
            Some(StopStage::Tacky)
        } else if options.validate {
            Some(StopStage::Analysis)
        } else if options.c {
            Some(StopStage::Object)
        } else {
            None
        }
    }
}

pub fn main() -> Result<()> {
    let args = Arguments::parse();

    let stop_stage = StopStage::from_args(&args.stage_options);

    run_driver(
        &args.path,
        &stop_stage,
        args.stage_options.debug,
        args.link_lib,
    )
}

fn run_driver(
    path: &str,
    stop_stage: &Option<StopStage>,
    debug: bool,
    link_lib: Option<String>,
) -> Result<()> {
    let dir_path = Path::new(path);

    // source code should always be inside a directory, but better error handling can come later
    let dir = dir_path.parent().unwrap();
    let file_name = dir_path
        .file_stem()
        .unwrap()
        .to_owned()
        .into_string()
        .unwrap();

    let pp_name = format!("{}/{file_name}.i", dir.display());
    let assembly_path = format!("{}/{file_name}.s", dir.display());

    // Preprocess input
    Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(dir_path)
        .arg("-o")
        .arg(&pp_name)
        .output()
        .expect("Failed to execute preprocessor process");

    // compile
    compile(&pp_name, stop_stage, &assembly_path, debug)?;

    //delete preprocessed file
    Command::new("rm")
        .arg(&pp_name)
        .output()
        .expect("Failed to delete preprocessed file");

    // Assemble only if we don't stop during compilation
    let object_path = format!("{}/{file_name}.o", dir.display());

    // Assemble but don't link
    let output = Command::new("gcc")
        .arg("-c")
        .arg(&assembly_path)
        .arg("-o")
        .arg(&object_path)
        .output()
        .expect("Failed to execute assembler");

    std::io::stdout().write_all(&output.stdout)?;
    std::io::stderr().write_all(&output.stderr)?;

    if let Some(StopStage::Object) = stop_stage {
        return Ok(());
    }

    // actually link files together
    let output = format!("{}/{file_name}", dir.display());

    let output = Command::new("gcc")
        .arg(&assembly_path)
        .arg("-o")
        .arg(&output)
        .arg(if let Some(lib) = link_lib {
            format!("-l{lib}")
        } else {
            "".into()
        })
        .output()
        .expect("Failed to execute assembler and linker");

    std::io::stdout().write_all(&output.stdout)?;
    std::io::stderr().write_all(&output.stderr)?;

    Ok(())
}

/// Actually run our compiler stages: Lexer, Parser, Codegen
/// If no StopStage is specified, an assembly file is outputted with a ".s" extension
/// Only Lexer, Parser, and Codegen StopStages are used in this function
fn compile(path: &str, stop_stage: &Option<StopStage>, assm_path: &str, debug: bool) -> Result<()> {
    // This function will be responsible for actually deciding whether to output any files

    let source =
        read_to_string(path).unwrap_or_else(|_| panic!("Unable to read source file: {}", path));

    let mut lexer = Lexer::new(&source);

    let (tokens, errors): (Vec<_>, Vec<_>) = lexer.tokenize().partition(|t| {
        t.kind != TokenType::Unknown
            && t.kind != TokenType::InvalidIdent
            && t.kind != TokenType::Error
    });

    if !errors.is_empty() {
        let mut error_msgs = Vec::new();

        for err in errors {
            error_msgs.push(format!(
                "{:?} at {}:{}:{}: '{}'",
                err.value,
                path.rsplit_once('/').unwrap().1,
                err.line,
                err.col,
                &source[err.start..err.end]
            ));
        }

        return Err(CompileErr::Lexer(error_msgs).into());
    }

    // println!("{:#?}", tokens);

    if let Some(StopStage::Lexer) = stop_stage {
        return Ok(());
    }

    let mut parser = Parser::new(tokens);
    let mut ast = parser.parse()?;

    // println!("AST:\n{:#?}", ast);

    if let Some(StopStage::Parser) = stop_stage {
        return Ok(());
    }

    resolve(&mut ast)?;

    validate_labels(&mut ast)?;

    label_loops(&mut ast)?;

    // println!("PRE-TYPE_CHECKER AST:\n{:#?}", ast);

    let mut type_checker = TypeChecker::new();
    type_checker.check(&mut ast)?;

    analyze_switches(&mut ast)?;

    // println!("RESOLVED AST:\n{:#?}", ast);

    if let Some(StopStage::Analysis) = stop_stage {
        return Ok(());
    }

    let tacky = gen_tacky(&ast, &mut type_checker.symbols);

    if debug {
        // println!("Assm Path: {}", assm_path);

        let tacky_name: Vec<_> = assm_path.split(".s").collect();
        let tacky_name = format!("{}.tacky", tacky_name.first().unwrap());

        debug_tacky(&tacky, tacky_name)?;
    }

    // println!("TACKY:\n{:#?}", tacky);

    if let Some(StopStage::Tacky) = stop_stage {
        return Ok(());
    }

    let assm_ast = gen_assm(&tacky, &mut type_checker.symbols);

    // println!("ASSM:\n{:#?}", assm_ast);

    if let Some(StopStage::CodeGen) = stop_stage {
        return Ok(());
    }

    output(assm_path, assm_ast, &type_checker.symbols)?;

    Ok(())
}

#[allow(dead_code)]
#[derive(Error, Debug)]
enum CompileErr {
    #[error("Lexer encountered an error(s): {:#?}", .0)]
    Lexer(Vec<String>),
    #[error("Parser encountered an error: {:#?}", .0)]
    Parser(Vec<String>),
    #[error("Codegen encountered an error: {:#?}", .0)]
    CodeGen(Vec<String>),
}
