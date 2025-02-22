/* Grammar
* program -> (line '\n')* line? $
* line -> (expr | command)
* expr -> term
* term -> factor (('+' | '-') factor)*
* factor -> primary (('*' | '/') primary)*
* primary -> NUMBER
* command -> ":" command arg*
*/

use std::cell::Cell;

use crate::scanner::{Tok, TokType};
use crate::line::*;
use crate::error::Error;

pub struct Parser<'a> {
    toks: Vec<Tok<'a>>,
    cur: Cell<usize>
}

impl<'a> Parser<'a> {
    pub fn new(toks: Vec<Tok<'a>>) -> Self {
        Self { toks, cur: Cell::new(0) }
    }
    fn peek(&self) -> &Tok<'a> {
        &self.toks[self.cur.get()]
    }
    fn is_match(&self, t: TokType) -> bool {
        self.peek().t == t
    }
    fn advance(&self) -> &Tok<'a> {
        let cur = self.cur.get();
        let tok = &self.toks[cur];
        if cur < self.toks.len() - 1 {
            self.cur.set(cur + 1);
        }
        tok
    }
    fn expect(&self, t: TokType, msg: String) -> Result<(), Error> {
        let tok = self.peek();
        if tok.t != t {
            Err(Error { msg, line: tok.line, col_start: tok.col_start, col_end: tok.col_end})
        } else{
            Ok(())
        }
    }
    fn expect_n(&self, t: &[TokType], msg: String) -> Result<(), Error> {
        let tok = self.peek();
        let mut is_match = false;
        for t in t.iter() {
            if tok.t == *t {
                is_match = true;
                break;
            }
        }
        if is_match {
            Ok(())
        } else {
            Err(Error { msg, line: tok.line, col_start: tok.col_start, col_end: tok.col_end})
        }
    }
    // primary -> NUMBER
    fn primary(&self) -> Result<Expr, Error> {
        let tok = self.peek();
        let lexeme = if tok.lexeme.get(0).is_some() && tok.lexeme[0] == b'\n' {
            String::from("end of line")
        } else {
            format!("'{}'", std::str::from_utf8(tok.lexeme).unwrap())
        };
        self.expect_n(
            &[TokType::Number],
            format!("Expected a number or variable, instead found {}.", lexeme)
        )?;
        Ok(Expr::Literal(self.advance().clone()))
    }
    // factor -> primary (('*' | '/') primary)*
    fn factor(&self) -> Result<Expr, Error> {
        let mut expr = self.primary()?;
        while self.is_match(TokType::Star) || self.is_match(TokType::Slash) {
            let op = (*self.advance()).clone();
            let right = self.primary()?;
            expr = Expr::Binary(Binary::new(expr, op, right));
        }
        Ok(expr)
    }
    // term -> factor (('+' | '-') factor)*
    fn term(&self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;
        while self.is_match(TokType::Plus) || self.is_match(TokType::Minus) {
            let op = self.advance().clone();
            let right = self.factor()?;
            expr = Expr::Binary(Binary::new(expr, op, right));
        }
        Ok(expr)
    }
    // expr -> term
    fn expr(&self) -> Result<Expr, Error> {
        self.term()
    }
    // TODO: command
    // line -> (expr | command)
    fn line(&self) -> Result<Line, Error> {
        self.expr().map(|expr| Line::Expr(expr))
    }
    fn skip_new_lines(&self) {
        while self.is_match(TokType::NewLine) {
            let _ = self.advance();
        }
    }
    // '\n'* line '\n'*
    pub fn parse_line(&self) -> Result<Line, Error> {
        self.skip_new_lines();
        let line = self.line()?;
        self.skip_new_lines();
        self.expect(TokType::EOF, String::from("expected end of line."))?;
        Ok(line)
    }
    // program -> '\n'* ((line | $) '\n'+)+
    pub fn parse(&self) -> Result<Vec<Line>, Error> {
        let mut lines= Vec::<Line>::new();
        self.skip_new_lines();
        while !self.is_match(TokType::EOF) {
            lines.push(self.line()?);
            self.expect(TokType::NewLine, String::from("expected a new line."))?;
            self.skip_new_lines();
        }
        return Ok(lines);
    }
}
