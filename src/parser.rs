/* Grammar
*
* program -> '\n'* ((statement | $) '\n'+)+
*
* statement -> ( expr | command | var | def | block | if | while | break | cont | set | proc |
                 return )
* var -> 'let' IDENTIFIER '=' expr
* def -> 'def' IDENTIFIER params_list '=' expr
* set -> 'set' IDENTIFIER (('[' expr ']')* '[' expr ']')? '=' expr
* if -> 'if' expr block ('else' (block | if))?
* while -> 'while' expr block
* proc -> 'proc' IDENTIFIER '(' (IDENTIFIER ( ',' IDENTIFIER )*)? ')' block
* 
* block -> '\n'* '{' '\n'* (statement '\n'+)* '}' '\n'
*
* expr -> term
* and -> or ("and" or)*
* or -> comp ("or" comp)*
* comp -> term ((">" | "<" | ">=" | "<=" | "=" | "!=") term)*
* term -> factor (('+' | '-') factor)*
* factor -> negate (('*' | '/') negate)*
* negate -> '-'? expo
* expo -> index ('^' expo )?
* index -> group ('[' expr ']')*
* group -> '(' expr ')' | call
* call -> IDENTIFIER args_list
* primary -> NUMBER | (IDENTIFIER | call) | BOOL | arr
* arr -> '[' (expr (',' expr )* )? ']'
*
* command -> ':' command arg*
* 
* params_list -> '(' IDENTIFIER (',' IDENTIFIER)* ')'
* args_list -> '(' (expr (',' expr )*)? ')'
*/

use std::cell::Cell;
use std::collections::HashSet;

use crate::scanner::{Tok, TokType};
use crate::statement::*;
use crate::error::{Error, Special};

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
    fn any_match(&self, t: &[TokType]) -> bool {
        let current = self.peek();
        for t in t {
            if current.t == *t {
                return true;
            }
        }
        false
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
            Err(Error { special: None, msg, line: tok.line, col_start: tok.col_start, col_end: tok.col_end})
        } else{
            Ok(())
        }
    }
    // args_list -> '(' (expr (',' expr )*)? ')'
    pub fn args_list(&self) -> Result<Vec<Expr>, Error> {
        let mut args = Vec::new();

        if !self.is_match(TokType::RParen) {
            args.push(self.expr()?);

            while self.is_match(TokType::Comma) {
                let _ = self.advance();
                args.push(self.expr()?);
            }
        }

        self.expect(TokType::RParen, String::from("Expected a closing parentheses after arguments."))?;
        let _ = self.advance();

        Ok(args)
    }
    // call -> IDENTIFIER args_list
    pub fn call(&self, identifier: Tok) -> Result<Expr, Error> {
        assert_eq!(self.advance().t, TokType::LParen);
        let args = self.args_list()?;
        Ok(Expr::Call(Call { identifier, args, rparen: self.toks[self.cur.get() - 1].clone() }))
    }
    // arr -> '[' ( expr (',' expr )* )? ']'
    fn arr(&self) -> Result<Expr, Error> {
        if self.is_match(TokType::RBrac) {
            let _ = self.advance();
            return Ok(Expr::Arr(Vec::new()))
        }

        let mut elements = vec![self.expr()?];
        while !self.is_match(TokType::RBrac) {
            self.expect(TokType::Comma, String::from("Expected comma or right bracket after array element."))?;
            self.advance();
            elements.push(self.expr()?);
        }
        let _ = self.advance();

        Ok(Expr::Arr(elements))
    }
    // primary -> NUMBER | (IDENTIFIER | call) | BOOL | arr
    fn primary(&self) -> Result<Expr, Error> {
        let next = self.advance().clone();
        let lexeme = if next.lexeme.as_bytes().get(0).map(|c| *c == b'\n').unwrap_or(false) {
            String::from("end of line")
        } else {
            format!("'{}'", next.lexeme)
        };
        match next.t {
            TokType::Number | TokType::True | TokType::False => {
                Ok(Expr::Literal(next))
            }
            TokType::Identifier => {
                if self.is_match(TokType::LParen) {
                    self.call(next)
                } else {
                    Ok(Expr::Literal(next))
                }
            }
            TokType::Str => {
                Ok(Expr::Literal(next))
            }
            TokType::LBrac => self.arr(),
            _ => return Err(Error {
                msg: format!("Expected a number or variable, instead found {}.", lexeme),
                line: next.line, col_start: next.col_start, col_end: next.col_end,
                special: None,
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
    // index -> group ('[' expr ']')*
    fn index(&self) -> Result<Expr, Error> {
        let mut expr = self.group()?;
        while self.is_match(TokType::LBrac) {
            let lb = self.advance().clone();
            let index = Box::new(self.expr()?);
            self.expect(TokType::RBrac, String::from("Expected closing bracket after index."))?;
            let rb = self.advance().clone();
            expr = Expr::Index(Index { expr: Box::new(expr), lb, index, rb });
        }
        Ok(expr)
    }
    // expo -> index ('^' expo )?
    fn expo(&self) -> Result<Expr, Error> {
        let mut expr = self.index()?;
        if self.is_match(TokType::Carrot) {
            let op = self.advance().clone();
            let power = Box::new(self.expo()?);
            expr = Expr::Exp(Exp { base: Box::new(expr), op, power });
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
        let expr = self.negate()?;
        if self.is_match(TokType::Star) || self.is_match(TokType::Slash) {
            let mut operands = vec![expr];
            let mut ops = Vec::new();
            while self.is_match(TokType::Star) || self.is_match(TokType::Slash) {
                ops.push(self.advance().clone());
                operands.push(self.negate()?);
            }
            Ok(Expr::Binary(Binary::new(ops, operands)))
        } else {
            Ok(expr)
        }
    }
    // term -> factor (('+' | '-') factor)*
    fn term(&self) -> Result<Expr, Error> {
        let expr = self.factor()?;
        if self.is_match(TokType::Plus) || self.is_match(TokType::Minus) {
            let mut operands = vec![expr];
            let mut ops = Vec::new();
            while self.is_match(TokType::Plus) || self.is_match(TokType::Minus) {
                ops.push(self.advance().clone());
                operands.push(self.factor()?);
            }
            Ok(Expr::Binary(Binary::new(ops, operands)))
        } else {
            Ok(expr)
        }
    }
    // comp -> term ((">" | "<" | ">=" | "<=" | "=" | "!=") term)*
    fn comp(&self) -> Result<Expr, Error> {
        let mut expr = self.term()?;
        use TokType::*;
        if self.any_match(&[Greater, GreaterEqual, Lesser, LesserEqual, Equal, BangEqual]) {
            let mut operators = Vec::new();
            let mut operands = vec![expr];
            while self.any_match(&[Greater, GreaterEqual, Lesser, LesserEqual, Equal, BangEqual]) {
                operators.push(self.advance().clone());
                operands.push(self.term()?);
            }
            expr = Expr::Comp(Comp { operators, operands });
        }
        Ok(expr)
    }
    // comp -> or ("and" comp)*
    fn and(&self) -> Result<Expr,Error> {
        let mut expr = self.comp()?;
        while self.is_match(TokType::And) {
            let op = self.advance().clone();
            expr = Expr::And(And { op, left: Box::new(expr), right: Box::new(self.comp()?) });
        }
        Ok(expr)
    }
    // or -> comp ("or" comp)*
    fn or(&self) -> Result<Expr,Error> {
        let mut expr = self.and()?;
        while self.is_match(TokType::Or) {
            let op = self.advance().clone();
            expr = Expr::Or(Or { op, left: Box::new(expr), right: Box::new(self.and()?) });
        }
        Ok(expr)
    }
    // expr -> and
    fn expr(&self) -> Result<Expr, Error> {
        self.or()
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
    fn set_helper(&self) -> Result<(Tok, Expr), Error> {
        self.expect(TokType::Equal, String::from("Expected the assignment operator '=' after the variable name."))?;
        let op = self.advance().clone();
        let value = self.expr()?;
        Ok((op, value))
    }
    // set -> 'set' (index | IDENTIFIER) '=' expr
    fn set(&self) -> Result<Statement, Error> {
        let set = self.advance();
        let target = self.index()?;
        match target {
            Expr::Literal(identifier) => {
                let (op, value) = self.set_helper()?;
                Ok(Statement::Set(Set { identifier, op, value }))
            }
            Expr::Index(index) => {
                let (op, value) = self.set_helper()?;
                Ok(Statement::SetIndex(SetIndex { index, op, value }))
            },
            _ => {
                let msg = String::from("Expected identifier after 'set' but found expression.");
                Err(Error{ special: None,
                    msg, line: set.line, col_start: set.col_start, col_end: set.col_end
                })
            }
        }
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
    // block -> '\n'* '{' '\n'* (statement '\n'+)* '}' '\n'
    fn block(&self) -> Result<Block, Error> {
        self.skip_new_lines();
        self.expect(TokType::LCurly, String::from("Expected opening curly brace."))?;
        let _ = self.advance();
        self.expect(TokType::NewLine, String::from("Expected new line."))?;
        let _ = self.advance();

        let mut statements = Vec::new();
        loop {
            self.skip_new_lines();
            if self.is_match(TokType::RCurly) {
                let _ = self.advance();
                break;
            }
            statements.push(self.statement()?);
            self.expect(TokType::NewLine, String::from("Expected new line."))?;
        }
        
        Ok(Block{ statements })
    }
    // if -> 'if' expr block ('else' (block | if))?
    fn eif(&self) -> Result<If, Error> {
        let eif = self.advance().clone();
        let cond = self.expr()?;
        let if_branch = Box::new(Statement::Block(self.block()?));
        let mut else_branch = None;
        if self.is_match(TokType::Else) {
            let _ = self.advance();
            if self.is_match(TokType::If) {
                else_branch = Some(Statement::If(self.eif()?));
            } else {
                else_branch = Some(Statement::Block(self.block()?));
            }
        }
        Ok(If {
            eif, cond, if_branch, else_branch: else_branch.map(|e| Box::new(e))
        })
    }
    fn hwile(&self) -> Result<While, Error> {
        let hwile = self.advance().clone();
        let cond = self.expr()?;
        let body = Box::new(Statement::Block(self.block()?));
        Ok(While { hwile, cond, body })
    }
    // proc -> 'proc' IDENTIFIER '(' (IDENTIFIER ( ',' IDENTIFIER )*)? ')' block
    fn proc(&self) -> Result<Proc, Error> {
        let _ = self.advance();
        self.expect(TokType::Identifier, String::from("Expected procedure name."))?;
        let name = self.advance().clone();
        self.expect(TokType::LParen, String::from("Expected opening parenthesis"))?;
        let _ = self.advance();

        let mut params = HashSet::new();

        if self.is_match(TokType::Identifier) {
            params.insert(self.advance().lexeme.clone());

            while self.is_match(TokType::Comma) {
                let _ = self.advance();
                self.expect(TokType::Identifier, String::from("Expected parameter name here."))?;

                let cur = self.advance();

                if params.contains(&cur.lexeme) {
                    let msg = String::from("Attempt to create duplicate parameter.");
                    return Err(Error {
                        msg, special: None, col_start: cur.col_start, col_end: cur.col_end, line: cur.line
                    })
                } else {
                    params.insert(cur.lexeme.clone());
                }
            }
        }

        self.expect(TokType::RParen, String::from("Expected closing parenthesis."))?;
        let _ = self.advance();

        let body = self.block()?;

        let params = params.into_iter().collect();
        Ok(Proc { name, params, body })
    }
    fn _break(&self) -> Error {
        let b = self.advance();
        let msg = String::from("Attempt to use break statement outside of loop");
        Error {
            msg, special: Some(Special::Break), col_start: b.col_start, col_end: b.col_end, line: b.line
        }
    }
    fn _continue(&self) -> Error {
        let b = self.advance();
        let msg = String::from("Attempt to use continue statement outside of loop");
        Error {
            msg, special: Some(Special::Continue), col_start: b.col_start, col_end: b.col_end, line: b.line
        }
    }
    fn _return(&self) -> Error {
        let r = self.advance();
        let value = if !self.is_match(TokType::NewLine) {
            Some(match self.expr() {
                Ok(e) => e,
                Err(e) => return e,
            })
        } else {
            None
        };
        let msg = String::from("Attempt to use return statement outside of procedure.");
        Error {
            msg, special: Some(Special::Return(value)), col_start: r.col_start, col_end: r.col_end, line: r.line
        }
    }
    // TODO: command
    // statement -> (expr | command | var | def | block | if | while | break | cont)
    fn statement(&self) -> Result<Statement, Error> {
        match self.peek().t {
            TokType::Let => self.var().map(|a| Statement::Var(a)),
            TokType::Set => self.set(),
            TokType::Def => self.def().map(|d| Statement::Def(d)),
            TokType::LCurly => self.block().map(|b| Statement::Block(b)),
            TokType::If => self.eif().map(|i| Statement::If(i)),
            TokType::While => self.hwile().map(|w| Statement::While(w)),
            TokType::Break => Ok(Statement::Break(self._break())),
            TokType::Continue => Ok(Statement::Continue(self._continue())),
            TokType::Return => Ok(Statement::Return(self._return())),
            TokType::Proc => self.proc().map(|p| Statement::Proc(p)),
            _ => self.expr().map(|e| Statement::Expr(e)),
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
        self.expect(TokType::EOF, String::from("Expected new line."))?;
        Ok(line)
    }
    // program -> '\n'* ((line | $) '\n'+)+
    pub fn parse(&self) -> Result<Vec<Statement>, Error> {
        let mut lines= Vec::<Statement>::new();
        self.skip_new_lines();
        while !self.is_match(TokType::EOF) {
            lines.push(self.statement()?);
            self.expect(TokType::NewLine, String::from("Expected a new line."))?;
            self.skip_new_lines();
        }
        return Ok(lines);
    }
}
