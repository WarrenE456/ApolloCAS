
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
        macro_rules! handle_error {
            ($x:expr, $l:expr) => {
                match $x {
                    Ok(line) => line,
                    Err(e) => {
                        e.display($l);
                        continue;
                    }
                }
            }
        }
        let interpreter = Interpreter::new();
        let mut lines: Vec<String> = Vec::new();
        let mut line_count = 0;
        loop {
            print!(">\t");
            let _ = stdout().flush();
            let mut line = String::new();
            stdin().read_line(&mut line).unwrap();
            lines.push(line.clone());
            line_count += 1;

            let scanner = Scanner::from(line.as_bytes(), line_count);
            let toks = handle_error!(scanner.scan(), &lines);

            let parser = Parser::new(toks);
            let statement = handle_error!(parser.parse_line(), &lines);
            
            let val = handle_error!(interpreter.interpret(statement), &lines);
            if let Some(val) = val {
                println!("{}", val);
            }
        }
    }
    fn run_file(args: Vec<String>) {
        macro_rules! handle_error {
            ($x:expr, $l:expr) => {
                match $x {
                    Ok(line) => line,
                    Err(e) => {
                        e.display($l);
                        exit(1);
                    }
                }
            }
        }

        let file_path = &args[1];
        let program = match read_to_string(file_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error: Failed to read file '{}'. {}", file_path, e.to_string()); 
                exit(1);
            }
        } + "\n";

        let program_lines = program
            .split("\n")
            .map(|s| s.to_owned())
            .collect::<Vec<_>>();
        let program = program.as_bytes();

        let scanner = Scanner::new(program);
        let toks = handle_error!(scanner.scan(), &program_lines);

        let parser = Parser::new(toks);
        let lines = handle_error!(parser.parse(), &program_lines);

        let interpreter = Interpreter::new();
        for line in lines.into_iter() {
           handle_error!(interpreter.interpret(line), &program_lines); 
        }
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
