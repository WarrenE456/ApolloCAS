use crate::parser::expr::Expr;
use crate::scanner::tok::Tok;

#[derive(Debug, Clone)]
pub enum Special {
    Break,
    Continue,
    Return(Option<Expr>, Tok),
    Exit(i32),
}

#[derive(Debug, Clone)]
pub struct Error {
    pub msg: String,
    pub line: usize,
    pub col_start: usize,
    pub col_end: usize,
    pub special: Option<Special>
}

impl Error {
    pub fn from(msg: String, start: &Tok, end: &Tok) -> Self {
        Self { special: None,
            msg, line: start.line, col_start: start.col_start, col_end: end.col_end
        }
    }
}

fn replace_tabs_w_4spaces(s: &String) -> String {
    s.bytes()
        .into_iter()
        .fold(String::new(), |acc, x| 
            if x == b'\t' {
                acc + "    "
            } else {
                let mut acc = acc;
                acc.push(x as char);
                acc
            }
        )
}

impl Error {
    pub fn display(&self, lines: &Vec<String>) {
        let line = replace_tabs_w_4spaces(&lines[self.line - 1])
            .trim_end()
            .to_owned();

        println!("Error on line {}: {}", self.line, self.msg);
        println!("{}", line);
        if self.col_start == self.col_end {
            println!("{}^", "-".repeat(self.col_start - 1));
        } else {
            println!("{}{}", " ".repeat(self.col_start - 1), "~".repeat(self.col_end - self.col_start + 1));
        }
    }
}
