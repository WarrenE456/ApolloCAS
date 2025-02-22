
use std::env::args;
use std::process::exit;
use std::fs::read_to_string;
use std::io::{stdin, stdout, Write};

use crate::scanner::Scanner;
use crate::parser::Parser;
use crate::interpreter::Interpreter;

pub struct Apollo {
}

// TODO: Error handling
impl Apollo {
    fn repl() {
        let interpreter = Interpreter::new();
        loop {
            print!(">\t");
            let _ = stdout().flush();
            let mut line = String::new();
            stdin().read_line(&mut line).unwrap();

            let scanner = Scanner::new(line.as_bytes());
            let toks = scanner.scan().unwrap();

            // TODO remove
            println!("{:?}", toks);

            let parser = Parser::new(toks);
            let line = parser.parse_line().unwrap();
            
            let val = interpreter.interpret(line).unwrap();
            println!("{}", val);
        }
    }
    fn run_file(args: Vec<String>) {
        let file_path = &args[1];
        let program = match read_to_string(file_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error: Failed to read file '{}'. {}", file_path, e.to_string()); 
                exit(1);
            }
        } + "\n";
        let program = program.as_bytes();
        let scanner = Scanner::new(program);
        let toks = scanner.scan().unwrap();
        println!("{:?}", toks);
        let parser = Parser::new(toks);
        let line = parser.parse().unwrap();
        println!("{:?}", line[0]);
    }
    pub fn run() {
        let args = args().collect::<Vec<_>>();
        if args.len() == 1 {
            Self::repl();
        }
        else if args.len() == 2 {
            Self::run_file(args);
        }
        else {
            eprintln!("Usage: apallo-cas [path/to/file]");
            exit(1);
        }

    }
}
