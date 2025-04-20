pub mod tok;

use std::cell::Cell;
use std::collections::HashMap;

use crate::error::Error;
use tok::*;

fn is_num(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

pub struct Scanner<'a> {
    p_pos: Cell<usize>,
    pos: Cell<usize>,
    line: Cell<usize>,
    col: Cell<usize>,
    program: &'a [u8],
    keywords: HashMap<String, TokType>,
}

impl<'a> Scanner<'a> {
    pub fn new(program: &'a [u8]) -> Self {
        Self::from(program, 1)
    }
    pub fn from(program: &'a [u8], lines: usize) -> Self {
        let keywords = HashMap::from([
            (String::from("let"), TokType::Let),
            (String::from("def"), TokType::Def),
            (String::from("or"), TokType::Or),
            (String::from("and"), TokType::And),
            (String::from("if"), TokType::If),
            (String::from("else"), TokType::Else),
            (String::from("while"), TokType::While),
            (String::from("break"), TokType::Break),
            (String::from("continue"), TokType::Continue),
            (String::from("set"), TokType::Set),
            (String::from("true"), TokType::True),
            (String::from("false"), TokType::False),
            (String::from("proc"), TokType::Proc),
            (String::from("return"), TokType::Return),
            (String::from("in"), TokType::In),
            (String::from("for"), TokType::For),
        ]);
        Scanner {
            program,
            pos: Cell::new(0),
            p_pos: Cell::new(0),
            line: Cell::new(lines),
            col: Cell::new(0),
            keywords
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
        let lexeme = std::str::from_utf8(&self.program[self.p_pos.get()..self.pos.get()])
            .unwrap()
            .to_owned();
        Tok { lexeme, line: self.line.get(), col_end: self.col.get(), col_start: self.col.get() - len, t }
    }
    fn make_tok_with_offset(&self, t: TokType, len: usize, offset: usize) -> Tok {
        let lexeme = std::str::from_utf8(&self.program[(self.p_pos.get() + offset)..self.pos.get()])
            .unwrap()
            .to_owned();
        Tok { lexeme, line: self.line.get(), col_end: self.col.get(), col_start: self.col.get() - len, t }
    }
    fn gen_error(&self, msg: String) -> Error {
        Error { msg, line: self.line.get(), col_start: self.col.get(), col_end: self.col.get(), special: None}
    }
    fn get_next_tok(&self) -> Option<Result<Tok, Error>> {
        // TODO macro to remove the Some(Ok... garbage
        use TokType::*;
        self.p_pos.set(self.pos.get());
        let c = self.advance();
        match c {
            b'+' => Some(Ok(self.make_tok(Plus, 0))),
            b'-' => Some(Ok(self.make_tok(Minus, 0))),
            b'*' => Some(Ok(self.make_tok(Star, 0))),
            b'/' => Some(Ok(self.make_tok(Slash, 0))),
            b'=' => Some(Ok(self.make_tok(Equal, 0))),
            b'(' => Some(Ok(self.make_tok(LParen, 0))),
            b')' => Some(Ok(self.make_tok(RParen, 0))),
            b'^' => Some(Ok(self.make_tok(Carrot, 0))),
            b',' => Some(Ok(self.make_tok(Comma, 0))),
            b'{' => Some(Ok(self.make_tok(LCurly, 0))),
            b'}' => Some(Ok(self.make_tok(RCurly, 0))),
            b'[' => Some(Ok(self.make_tok(LBrac, 0))),
            b']' => Some(Ok(self.make_tok(RBrac, 0))),
            b'<' => {
                if self.is_match(b'=') {
                    Some(Ok(self.make_tok(LesserEqual, 1)))
                } else {
                    Some(Ok(self.make_tok(Lesser, 0)))
                }
            }
            b'!' => {
                if self.is_match(b'=') {
                    Some(Ok(self.make_tok(BangEqual, 1)))
                } else {
                    Some(Ok(self.make_tok(Bang, 0)))
                }
            }
            b'>' => {
                if self.is_match(b'=') {
                    Some(Ok(self.make_tok(GreaterEqual, 1)))
                } else {
                    Some(Ok(self.make_tok(Greater, 1)))
                }
            }
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
                    Some(Ok(self.make_tok(Float, len)))
                } else {
                    Some(Ok(self.make_tok(Int, len)))
                }
            }
            b'"' => {
                let _ = self.advance();
                let mut len = 0;
                while self.peek() != b'"' {
                    if self.peek() == b'\n' {
                        return Some(Err(
                            self.gen_error(String::from("Expected closing quote to end string.")))
                        );
                    }
                    let _ = self.advance();
                    len += 1;
                }
                let tok = self.make_tok_with_offset(Str, len, 1);
                let _ = self.advance();
                Some(Ok(tok))
            }
            b' ' | b'\r' => {
                self.get_next_tok()
            }
            b'\t' => {
                self.col.set(self.col.get() + 3);
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
                while valid_identifier_character(self.peek()) {
                    len += 1;
                    _ = self.advance();
                }
                let mut tok = self.make_tok(Identifier, len);
                if let Some(t) = self.keywords.get(&tok.lexeme) {
                    tok.t = t.clone();
                }
                Some(Ok(tok))
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
