use clap::{Parser as ClapParser, Subcommand};
use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;
use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;

#[derive(ClapParser)]
#[command(name = "ryth")]
#[command(about = "Ryth - An extremely concise language for code golfing", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    #[arg(value_name = "FILE")]
    file: Option<PathBuf>,
}

#[derive(Subcommand)]
enum Commands {
    Run { file: Option<PathBuf> },
    Lex { file: Option<PathBuf> },
    Parse { file: Option<PathBuf> },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Run { file }) => {
            if let Err(e) = run_file(file) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
        Some(Commands::Lex { file }) => {
            if let Err(e) = lex_file(file) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
        Some(Commands::Parse { file }) => {
            if let Err(e) = parse_file(file) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
        None => {
            // If a file is provided without a subcommand, run it
            if let Some(file) = cli.file {
                if let Err(e) = run_file(Some(file)) {
                    eprintln!("Error: {}", e);
                    std::process::exit(1);
                }
            } else {
                // No file provided, read from stdin
                if let Err(e) = run_file(None) {
                    eprintln!("Error: {}", e);
                    std::process::exit(1);
                }
            }
        }
    }
}

fn read_source(file: Option<PathBuf>) -> io::Result<String> {
    match file {
        Some(path) => fs::read_to_string(path),
        None => {
            let mut buffer = String::new();
            io::stdin().read_to_string(&mut buffer)?;
            Ok(buffer)
        }
    }
}

fn run_file(file: Option<PathBuf>) -> Result<(), Box<dyn std::error::Error>> {
    let source = read_source(file)?;

    let lexer = Lexer::new(&source);
    let tokens: Result<Vec<_>, _> = lexer.collect();
    let tokens = tokens.map_err(|e| format!("Lexer error: {:?}", e))?;

    let mut parser = Parser::new(tokens);
    let program = parser
        .parse()
        .map_err(|e| format!("Parser error: {:?}", e))?;

    let mut interpreter = Interpreter::new();
    let results = interpreter
        .eval_program(program)
        .map_err(|e| format!("Interpreter error: {}", e))?;

    for result in results {
        match result {
            interpreter::Value::List(items) => {
                for item in items {
                    println!("{}", item);
                }
            }
            _ => println!("{}", result),
        }
    }

    Ok(())
}

fn lex_file(file: Option<PathBuf>) -> Result<(), Box<dyn std::error::Error>> {
    let source = read_source(file)?;

    let lexer = Lexer::new(&source);
    for token in lexer {
        match token {
            Ok(t) => println!("{:?}", t),
            Err(e) => {
                eprintln!("Lexer error: {:?}", e);
                std::process::exit(1);
            }
        }
    }

    Ok(())
}

fn parse_file(file: Option<PathBuf>) -> Result<(), Box<dyn std::error::Error>> {
    let source = read_source(file)?;

    let lexer = Lexer::new(&source);
    let tokens: Result<Vec<_>, _> = lexer.collect();
    let tokens = tokens.map_err(|e| format!("Lexer error: {:?}", e))?;

    let mut parser = Parser::new(tokens);
    let program = parser
        .parse()
        .map_err(|e| format!("Parser error: {:?}", e))?;

    println!("{:#?}", program);

    Ok(())
}
