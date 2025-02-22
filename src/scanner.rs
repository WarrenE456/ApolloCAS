use std::cell::Cell;
use crate::error::Error;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokType {
    // Single characters
    Plus, Minus, Star, Slash,
    // Single or possibly two characters
    Assign,
    // Variable number of characters
    Number, Identifier,
    // Msc.
    NewLine, EOF
}

// TODO: start col and end call
#[derive(PartialEq, Eq, Clone)]
pub struct Tok<'a> {
    pub lexeme: &'a [u8],
    pub line: usize,
    pub col_start: usize,
    pub col_end: usize,
    pub t: TokType,
}

impl<'a> Tok<'a> {
    pub fn lexeme_as_str(&self) -> &str {
        std::str::from_utf8(self.lexeme).unwrap()
    }
}

use std::fmt;
impl<'a> fmt::Debug for Tok<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Tok {{ ({},{}-{}) '{}' {:?} }}", self.line, self.col_start, self.col_end, self.lexeme_as_str(), self.t)
    }
}

fn is_num(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

pub struct Scanner<'a> {
    p_pos: Cell<usize>,
    pos: Cell<usize>,
    line: Cell<usize>,
    col: Cell<usize>,
    program: &'a [u8],
}

impl<'a> Scanner<'a> {
    pub fn new(program: &'a [u8]) -> Self {
        Scanner {
            program,
            pos: Cell::new(0),
            p_pos: Cell::new(0),
            line: Cell::new(1),
            col: Cell::new(0),
        }
    }
    fn advance(&self) -> u8 {
        let pos = self.pos.get();
        return if pos < self.program.len() {
            self.col.set(self.col.get() + 1);
            self.pos.set(pos + 1);
            self.program[pos]
        } else {
            b'\0'
        }
    }
    fn peek(&self) -> u8 {
        let pos = self.pos.get();
        return if pos < self.program.len() {
            self.program[pos]
        } else {
            b'\0'
        }
    }
    fn is_match(&self, c: u8) -> bool {
        return if self.peek() == c {
            _ = self.advance();
            true
        } else {
            false
        }
    }
    fn make_tok(&self, t: TokType, len: usize) -> Tok {
        let lexeme = &self.program[self.p_pos.get()..self.pos.get()];
        Tok { lexeme, line: self.line.get(), col_end: self.col.get(), col_start: self.col.get() - len, t }
    }
    fn gen_error(&self, msg: String) -> Error {
        Error { msg, line: self.line.get(), col_start: self.col.get(), col_end: self.col.get()}
    }
    fn get_next_tok(&self) -> Option<Result<Tok, Error>> {
        use TokType::*;
        self.p_pos.set(self.pos.get());
        let c = self.advance();
        match c {
            b'+' => Some(Ok(self.make_tok(Plus, 0))),
            b'-' => Some(Ok(self.make_tok(Minus, 0))),
            b'*' => Some(Ok(self.make_tok(Star, 0))),
            b'/' => Some(Ok(self.make_tok(Slash, 0))),
            b':' => {
                if self.is_match(b'=') {
                    let _ = self.advance();
                    Some(Ok(self.make_tok(Assign, 1)))
                } else {
                    Some(Err(self.gen_error(format!("Unexpected character '{}'.", c as char))))
                }
            },
            b'0'..=b'9' => {
                let mut len = 0;
                while is_num(self.peek()) {
                    _ = self.advance();
                    len += 1;
                }
                if self.is_match(b'.') {
                    len += 1;
                    while is_num(self.peek()) {
                        _ = self.advance();
                        len += 1;
                    }
                }
                Some(Ok(self.make_tok(Number, len)))
            }
            b' ' | b'\r' | b'\t' => {
                self.get_next_tok()
            }
            b'\n' => {
                let tok = Some(Ok(self.make_tok(NewLine, 0)));
                self.line.set(self.line.get() + 1);
                self.col.set(0);
                tok
            }
            b'\0' => {
                None
            }
            b'a'..=b'z' | b'A'..=b'Z' => {
                let mut len = 0;
                while (valid_identifier_character(self.peek())) {
                    len += 1;
                    _ = self.advance();
                }
                Some(Ok(self.make_tok(Identifier, len)))
            }
            _ => {
                Some(Err(self.gen_error(format!("Unexpected character '{}'.", c as char))))
            }
        }
    }
    pub fn scan(&self) -> Result<Vec<Tok>, Error> {
        let mut toks = Vec::<Tok>::new();
        loop {
            if let Some(tok) = self.get_next_tok() {
                toks.push(tok?);
            } else {
                break;
            }
        }
        self.p_pos.set(self.pos.get() );
        toks.push(self.make_tok(TokType::EOF, 0));
        Ok(toks)
    }
}

fn valid_identifier_character(c: u8) -> bool {
       b'a' <= c && c <= b'z' 
    || b'A' <= c && c <= b'Z' 
    || b'0' <= c && c <= b'9' 
    || c == b'_'
}
