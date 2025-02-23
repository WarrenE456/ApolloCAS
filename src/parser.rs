/* Grammar
* program -> '\n'* ((statement | $) '\n'+)+
* statement -> (expr | command | assignment)
* assignment -> "let" IDENTIFIER ':=' expr
* expr -> term
* term -> factor (('+' | '-') factor)*
* factor -> expo (('*' | '/') expo)*
* expo -> negate ('^' expo)?
* negate -> '-'? group
* group -> "(" expr ")" | primary
* primary -> NUMBER | IDENTIFIER
* command -> ":" command arg*
*/

use std::cell::Cell;

use crate::scanner::{Tok, TokType};
use crate::statement::*;
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
    // primary -> NUMBER | IDENTIFIER
    fn primary(&self) -> Result<Expr, Error> {
        let tok = self.peek();
        let lexeme = if tok.lexeme.get(0).is_some() && tok.lexeme[0] == b'\n' {
            String::from("end of line")
        } else {
            format!("'{}'", std::str::from_utf8(tok.lexeme).unwrap())
        };
        self.expect_n(
            &[TokType::Number, TokType::Identifier],
            format!("Expected a number or variable, instead found {}.", lexeme)
        )?;
        Ok(Expr::Literal(self.advance().clone()))
    }
    // group -> "(" expr ")" | primary
    fn group(&self) -> Result<Expr, Error> {
        if self.is_match(TokType::LPAREN) {
            let _ = self.advance();
            let expr = self.expr()?;
            self.expect(TokType::RPAREN, String::from("Expected a closing parentheses"))?;
            let _ = self.advance();
            Ok(expr)
        } else {
            self.primary()
        }
    }
    // negate -> '-'? group
    fn negate(&self) -> Result<Expr, Error> {
        if self.is_match(TokType::Minus) {
            let minus = self.advance().clone();
            self.group().map(|e| Expr::Negate(Negate{ minus, value: Box::new(e) }))
        } else {
            self.group()
        }
    }
    // expo -> negate ('^' expo)?
    fn expo(&self) -> Result<Expr, Error> {
        let mut expr = self.negate()?;
        if self.is_match(TokType::Carrot) {
            let op = self.advance().clone();
            let r = Box::new(self.expo()?);
            expr = Expr::Binary(Binary { l: Box::new(expr), op, r });
        }
        Ok(expr)
    }
    // factor -> expo (('*' | '/') expo)*
    fn factor(&self) -> Result<Expr, Error> {
        let mut expr = self.expo()?;
        while self.is_match(TokType::Star) || self.is_match(TokType::Slash) {
            let op = (*self.advance()).clone();
            let right = self.expo()?;
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
    // assignment -> "let" IDENTIFIER ':=' expr
    fn assignment(&self) -> Result<Assignment, Error> {
        assert_eq!(self.advance().t, TokType::Let);
        self.expect(TokType::Identifier, String::from("Expected a variable name here."))?;
        let identifier = self.advance().clone();
        self.expect(TokType::Assign, String::from("Expected the assignment operator ':=' after the variable name."))?;
        let op = self.advance().clone();
        let value = self.expr()?;
        Ok(Assignment { identifier, op, value })
    }
    // TODO: command
    // statement -> (expr | command | assignment)
    fn statement(&self) -> Result<Statement, Error> {
        if self.is_match(TokType::Let) {
            self.assignment().map(|a| Statement::Assignment(a))
        } else {
            self.expr().map(|e| Statement::Expr(e))
        }
    }
    fn skip_new_lines(&self) {
        while self.is_match(TokType::NewLine) {
            let _ = self.advance();
        }
    }
    // '\n'* line '\n'*
    pub fn parse_line(&self) -> Result<Statement, Error> {
        self.skip_new_lines();
        let line = self.statement()?;
        self.skip_new_lines();
        self.expect(TokType::EOF, String::from("expected new line."))?;
        Ok(line)
    }
    // program -> '\n'* ((line | $) '\n'+)+
    pub fn parse(&self) -> Result<Vec<Statement>, Error> {
        let mut lines= Vec::<Statement>::new();
        self.skip_new_lines();
        while !self.is_match(TokType::EOF) {
            lines.push(self.statement()?);
            self.expect(TokType::NewLine, String::from("expected a new line."))?;
            self.skip_new_lines();
        }
        return Ok(lines);
    }
}
