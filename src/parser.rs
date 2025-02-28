/* Grammar
*
* program -> '\n'* ((statement | $) '\n'+)+
*
* statement -> (expr | command | var | def)
* var -> 'let' IDENTIFIER '=' expr
* def -> 'def' IDENTIFIER params_list '=' expr
*
* expr -> term
* term -> factor (('+' | '-') factor)*
* factor -> negate (('*' | '/') negate)*
* negate -> '-'? expo
* expo -> group ('^' expo)?
* group -> '(' expr ')' | primary
* call -> IDENTIFIER args_list
* primary -> NUMBER | (IDENTIFIER | call)
*
* command -> ':' command arg*
* 
* params_list -> '(' IDENTIFIER (',' IDENTIFIER)* ')'
* args_list -> '(' expr (',' expr )* ')'
*/

use std::cell::Cell;

use crate::scanner::{Tok, TokType};
use crate::statement::*;
use crate::error::Error;

pub struct Parser {
    toks: Vec<Tok>,
    cur: Cell<usize>
}

impl Parser {
    pub fn new(toks: Vec<Tok>) -> Self {
        Self { toks, cur: Cell::new(0) }
    }
    fn peek(&self) -> &Tok {
        &self.toks[self.cur.get()]
    }
    fn is_match(&self, t: TokType) -> bool {
        self.peek().t == t
    }
    fn advance(&self) -> &Tok {
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
    // args_list -> '(' expr (',' expr )* ')'
    pub fn args_list(&self) -> Result<Vec<Expr>, Error> {
        let mut args = Vec::new();
        args.push(self.expr()?);

        while self.is_match(TokType::Comma) {
            let _ = self.advance();
            args.push(self.expr()?);
        }

        self.expect(TokType::RParen, String::from("Expected a closing parentheses after arguments."))?;
        let _ = self.advance();

        Ok(args)
    }
    // call -> IDENTIFIER args_list
    pub fn call(&self, identifier: Tok) -> Result<Expr, Error> {
        assert_eq!(self.advance().t, TokType::LParen);
        let args = self.args_list()?;
        Ok(Expr::Call(Call { identifier, args, lparen: self.toks[self.cur.get() - 1].clone() }))
    }
    // primary -> NUMBER | (IDENTIFIER | call)
    fn primary(&self) -> Result<Expr, Error> {
        let next = self.advance().clone();
        let lexeme = if next.lexeme.as_bytes().get(0).map(|c| *c == b'\n').unwrap_or(false) {
            String::from("end of line")
        } else {
            format!("'{}'", next.lexeme)
        };
        match next.t {
            TokType::Number => {
                Ok(Expr::Literal(next))
            }
            TokType::Identifier => {
                if self.is_match(TokType::LParen) {
                    self.call(next)
                } else {
                    Ok(Expr::Literal(next))
                }
            }
            _ => return Err(Error {
                msg: format!("Expected a number or variable, instead found {}.", lexeme),
                line: next.line, col_start: next.col_start, col_end: next.col_end,
            }),
        }
    }
    // group -> "(" expr ")" | primary
    fn group(&self) -> Result<Expr, Error> {
        if self.is_match(TokType::LParen) {
            let _ = self.advance();
            let expr = self.expr()?;
            self.expect(TokType::RParen, String::from("Expected a closing parentheses"))?;
            let _ = self.advance();
            Ok(Expr::Group(Box::new(expr)))
        } else {
            self.primary()
        }
    }
    // expo -> group ('^' expo)?
    fn expo(&self) -> Result<Expr, Error> {
        let mut expr = self.group()?;
        if self.is_match(TokType::Carrot) {
            let op = self.advance().clone();
            let r = Box::new(self.expo()?);
            expr = Expr::Binary(Binary { l: Box::new(expr), op, r });
        }
        Ok(expr)
    }
    // negate -> '-'? expo
    fn negate(&self) -> Result<Expr, Error> {
        if self.is_match(TokType::Minus) {
            let minus = self.advance().clone();
            self.expo().map(|e| Expr::Negate(Negate{ minus, value: Box::new(e) }))
        } else {
            self.expo()
        }
    }
    // factor -> negate (('*' | '/') negate)*
    fn factor(&self) -> Result<Expr, Error> {
        let mut expr = self.negate()?;
        while self.is_match(TokType::Star) || self.is_match(TokType::Slash) {
            let op = (*self.advance()).clone();
            let right = self.negate()?;
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
    // var -> "let" IDENTIFIER '=' expr
    fn var(&self) -> Result<Var, Error> {
        assert_eq!(self.advance().t, TokType::Let);
        self.expect(TokType::Identifier, String::from("Expected a variable name here."))?;
        let identifier = self.advance().clone();
        self.expect(TokType::Equal, String::from("Expected the assignment operator '=' after the variable name."))?;
        let op = self.advance().clone();
        let value = self.expr()?;
        Ok(Var { identifier, op, value })
    }
    // params_list -> '(' IDENTIFIER (',' IDENTIFIER)* ')'
    pub fn params_list(&self) -> Result<Vec<String>, Error> {
        self.expect(TokType::LParen, String::from("Expected a parentheses before parameter list."))?;
        let _ = self.advance();

        let mut args = Vec::new();
        self.expect(TokType::Identifier, String::from("Expected an parameter here."))?;
        args.push(self.advance().lexeme.clone());

        while self.is_match(TokType::Comma) {
            let _ = self.advance();
            self.expect(TokType::Identifier, String::from("Expected an parameter to follow the comma."))?;
            args.push(self.advance().lexeme.clone());
        }

        self.expect(TokType::RParen, String::from("Expected a closing parentheses after parameters."))?;
        let _ = self.advance();

        Ok(args)
    }
    // def -> 'def' IDENTIFIER params_list '=' expr
    fn def(&self) -> Result<Def, Error> {
        assert_eq!(self.advance().t, TokType::Def);
        self.expect(TokType::Identifier, String::from("Expected a function name here."))?;
        let identifier = self.advance().clone();

        let args = self.params_list()?;

        self.expect(TokType::Equal, String::from("Expected the assignment operator '=' after the variable name."))?;
        let op = self.advance().clone();

        Ok(Def { identifier, args, op, value: self.expr()? })
    }
    // TODO: command
    // statement -> (expr | command | var | def)
    fn statement(&self) -> Result<Statement, Error> {
        if self.is_match(TokType::Let) {
            self.var().map(|a| Statement::Var(a))
        } else if self.is_match(TokType::Def) {
            self.def().map(|d| Statement::Def(d))
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
