mod interpreter;
mod ast;
mod lexer;
mod parser;

use std::{env, fs};

use interpreter::{Compile, Engine};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("No input file was provided.");
        std::process::exit(-1);
    }

    println!(
        "{:?}",
        Engine::from_source(&fs::read_to_string(&args[1]).unwrap()).unwrap()
    );
}
