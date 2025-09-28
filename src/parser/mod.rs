/* Grammar
*
* program -> '\n'* ((statement | $) '\n'+)+
*
* statement -> ( expr | command | var | def | block | if | while | break | cont | set |
                 return | for )

* var -> 'let' typed '=' expr
* def -> 'def' IDENTIFIER params_list '=' expr
* set -> 'set' IDENTIFIER (('[' expr ']')* '[' expr ']')? '=' expr
* if -> 'if' expr block ('else' (block | if))?
* while -> 'while' expr block
* for -> 'for' IDENTIFIER 'in' expr block
* 
* block -> '\n'* '{' '\n'* (statement '\n'+)* '}' '\n'
* inline_ret -> ':' '\n'? expr '\n'
* type -> Any | Int | Float | Fn | BuiltIn | Bool | Unit | Str | Arr | Char | fn_t | ...
* typed -> IDENTIFIER (':' type)?
* fn_t -> '(' type ( ',' type )* '->' type ')'
*
* expr -> '$' SYMEXPR | or | _fn
* _fn -> '_fn' '(' (typed ( ',' typed )*)? ')' ("->" type)? (inline_ret | block)
* or -> comp ("or" comp)*
* and -> or ("and" or)*
* comp -> concat ((">" | "<" | ">=" | "<=" | "=" | "!=") concat)*
* concat -> term ('++' term)*
* term -> factor (('+' | '-') factor)*
* factor -> negate (('*' | '/') negate)*
* negate -> '-'? expo
* expo -> index ('^' expo )?
* index -> group ('[' expr ']')*
* group -> '(' expr ')' | call
* call -> IDENTIFIER args_list
* primary -> NUMBER | (IDENTIFIER | call) | BOOL | arr | "()"
* arr -> '[' (expr (',' expr )* )? ']'
*
* command -> ':' command arg*
* 
* params_list -> '(' IDENTIFIER (',' IDENTIFIER)* ')'
* args_list -> '(' (expr (',' expr )*)? ')'
*/

pub mod statement;
pub mod expr;

use std::cell::Cell;
use std::collections::HashSet;

use crate::scanner::tok::{Tok, TokType};
use crate::parser::statement::*;
use crate::parser::expr::*;
use crate::error::{Error, Special};
use crate::parser::expr::Expr;
use crate::runtime::val::{Type, SymT};

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
    // primary -> NUMBER | (IDENTIFIER | call) | BOOL | arr | "()"
    fn primary(&self) -> Result<Expr, Error> {
        let next = self.advance().clone();
        let lexeme = if next.lexeme.as_bytes().get(0).map(|c| *c == b'\n').unwrap_or(false) {
            String::from("end of line")
        } else {
            format!("'{}'", next.lexeme)
        };
        match next.t {
            TokType::Float | TokType::Int | TokType::True | TokType::False | TokType::Str | TokType::Char | TokType::Unit => {
                Ok(Expr::Literal(next))
            }
            TokType::Identifier => {
                if self.is_match(TokType::LParen) {
                    self.call(next)
                } else {
                    Ok(Expr::Literal(next))
                }
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
            expr = Expr::Exp(Exp { base: Box::new(expr), op, exp: power });
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
    fn is_term_op(&self) -> bool {
        self.is_match(TokType::Plus) || self.is_match(TokType::Minus)
    }
    // term -> factor (('+' | '-') factor)*
    fn term(&self) -> Result<Expr, Error> {
        let expr = self.factor()?;
        if self.is_term_op() {
            let mut operands = vec![expr];
            let mut ops = Vec::new();
            while self.is_term_op() {
                ops.push(self.advance().clone());
                operands.push(self.factor()?);
            }
            Ok(Expr::Binary(Binary::new(ops, operands)))
        } else {
            Ok(expr)
        }
    }
    // concat -> term ('++' term)*
    fn concat(&self) -> Result<Expr, Error> {
        let mut expr = self.term()?;
        while self.is_match(TokType::PlusPlus) {
            let op = self.advance().clone();
            let r = Box::new(self.term()?);
            expr = Expr::Concat(Concat { l: Box::new(expr), op, r})
        }
        Ok(expr)
    }
    // comp -> concat ((">" | "<" | ">=" | "<=" | "=" | "!=") concat)*
    fn comp(&self) -> Result<Expr, Error> {
        let mut expr = self.concat()?;
        use TokType::*;
        if self.any_match(&[Greater, GreaterEqual, Lesser, LesserEqual, Equal, BangEqual]) {
            let mut operators = Vec::new();
            let mut operands = vec![expr];
            while self.any_match(&[Greater, GreaterEqual, Lesser, LesserEqual, Equal, BangEqual]) {
                operators.push(self.advance().clone());
                operands.push(self.concat()?);
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
    // expr -> $ SYM_EXPR | ...
    fn expr(&self) -> Result<Expr, Error> {
        if self.is_match(TokType::Dollar) {
            let dollar = self.advance().clone();
            self.expr().map(|e| Expr::Sym(dollar, Box::new(e)))
        }
        else if self.is_match(TokType::Fn) {
            self._fn().map(|f| Expr::Fn(f))
        } else {
            self.or()
        }
    }
    // fn_t -> '(' type ( ',' type )* '->' type ')'
    fn fn_t(&self) -> Result<Type, Error> {
        let mut param_t = vec![self.parse_type()?];
        while self.is_match(TokType::Comma) {
            let _ = self.advance();
            param_t.push(self.parse_type()?);
        }
        self.expect(TokType::Arrow, String::from("Expected an arrow after the parameter types and before the return type."))?;
        let _ = self.advance();
        let return_t = self.parse_type()?;
        self.expect(TokType::RParen, String::from("Expected a closing parenthesis."))?;
        let _ = self.advance();
        Ok(Type::Fn(param_t, Box::new(return_t)))
    }
    fn poly_t(&self, p: &Tok) -> Result<Type, Error> {
        use crate::sym::SymExpr;
        if p.lexeme == "P" {
            self.expect(TokType::LBrac, String::from("Expected left bracet after type 'P' (e.g. P[x])."))?;
            let lb = self.advance();
            let expr = self
                .expr()?
                .to_sym()
                .map_err(|msg| Error::from(msg, p, p))?;
            let var = match expr {
                SymExpr::Symbol(s) => s,
                other => {
                    let msg = format!(
                        "Expected a symbol between the brackets, but found a(n) {}",
                        other.kind_name()
                    );
                    return Err(Error::from(msg, lb, lb));
                }
            };
            self.expect(TokType::RBrac, String::from("Expected right bracket after symbol (e.g. P[x])."))?;
            let _ = self.advance();

            Ok(Type::Sym(SymT::Polynomial(var)))
        } else {
            todo!()
        }
    }
    // type -> Any | Int | Float | Fn | BuiltIn | Bool | Unit | Str | Arr | Char | fn_t | *T...
    fn parse_type(&self) -> Result<Type, Error> {
        let next = self.advance();
        use TokType::*;
        match next.t {
            AnyT => Ok(Type::Any),
            IntT => Ok(Type::Int),
            FloatT => Ok(Type::Float),
            BuiltInT => Ok(Type::BuiltIn),
            BoolT => Ok(Type::Bool),
            UnitT => Ok(Type::Unit),
            StrT => Ok(Type::Str),
            ArrT => Ok(Type::Arr),
            CharT => Ok(Type::Char),
            ZT => Ok(Type::Sym(SymT::Z)),
            SymAnyT => Ok(Type::Sym(SymT::Any)),
            SymbolT => Ok(Type::Sym(SymT::Symbol)),
            IterT => Ok(Type::Iter),
            LParen => self.fn_t(),
            Identifier => self.poly_t(next),
            _ => {
                let msg = String::from("Expected type here.");
                Err(Error { special: None,
                    msg, line: next.line, col_start: next.col_start, col_end: next.col_end
                })
            }
        }
    }
    fn typed(&self) -> Result<(Tok, Option<Type>), Error> {
        let identifier = self.advance().clone();

        let t = if self.is_match(TokType::Colon) {
            let _ = self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        Ok((identifier, t))
    }
    // var -> 'let' IDENTIFIER (':' type)? '=' expr
    fn var(&self) -> Result<Var, Error> {
        assert_eq!(self.advance().t, TokType::Let);
        self.expect(TokType::Identifier, String::from("Expected a variable name here."))?;

        let (identifier, t) = self.typed()?;
        let t = t.unwrap_or(Type::Auto);

        self.expect(TokType::Equal, String::from("Expected the assignment operator '=' after the variable name."))?;
        let op = self.advance().clone();
        let value = self.expr()?;
        Ok(Var { identifier, op, value, t })
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
    // inline_ret -> : '\n'? expr
    fn inline_ret(&self) -> Result<Expr, Error> {
        self.expect(TokType::Colon, String::from("Expected colon before inline return."))?;
        let _ = self.advance();
        self.skip_new_lines();
        self.expr()
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
    // _fn -> '_fn' '(' (typed ( ',' typed )*)? ')' ("->" type)? (block | inline_ret)
    fn _fn(&self) -> Result<Fn, Error> {
        let _ = self.advance();

        self.expect(TokType::LParen, String::from("Expected opening parenthesis"))?;
        let _ = self.advance();

        let mut param_names = HashSet::new();
        let mut params = Vec::new();

        if self.is_match(TokType::Identifier) {
            let (identifier, t) = self.typed()?;
            let t = t.unwrap_or(Type::Any);
            param_names.insert(identifier.lexeme.clone());
            params.push((identifier, t));

            while self.is_match(TokType::Comma) {
                let _ = self.advance();
                self.expect(TokType::Identifier, String::from("Expected parameter name here."))?;

                let (identifier, t) = self.typed()?;
                let t = t.unwrap_or(Type::Any);

                if param_names.contains(&identifier.lexeme) {
                    let msg = String::from("Attempt to create duplicate parameter.");
                    return Err(Error {
                        msg, special: None, col_start: identifier.col_start, col_end: identifier.col_end, line: identifier.line
                    })
                } else {
                    param_names.insert(identifier.lexeme.clone());
                    params.push((identifier, t));
                }
            }
        }

        self.expect(TokType::RParen, String::from("Expected closing parenthesis."))?;
        let _ = self.advance();

        let return_t = if self.is_match(TokType::Arrow) {
            let _ = self.advance();
            self.parse_type()?
        } else {
            Type::Unit
        };

        let body = if self.is_match(TokType::Colon) {
            let colon = self.peek().clone();
            let _return = Statement::Return(Error::new_return(Some(self.inline_ret()?), colon));
            Block { statements: vec![_return]}
        } else {
            self.block()?
        };

        Ok(Fn { params, return_t, body })
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
        Error::new_return(value, r.clone())
    }
    // for -> 'for' IDENTIFIER 'in' expr block
    fn _for(&self) -> Result<Statement, Error> {
        let fro = self.advance().clone();
        self.expect(TokType::Identifier, String::from("Expected a variable name after 'for'."))?;
        let identifier = self.advance().clone();
        self.expect(TokType::In, String::from("Expected 'in' after variable name."))?;
        let _ = self.advance();
        let iter = self.expr()?;
        let body = self.block()?;
        Ok(Statement::For(For { fro, identifier, iter, body }))
    }
    // TODO: command
    fn statement(&self) -> Result<Statement, Error> {
        match self.peek().t {
            TokType::Let => self.var().map(|a| Statement::Var(a)),
            TokType::Set => self.set(),
            TokType::LCurly => self.block().map(|b| Statement::Block(b)),
            TokType::If => self.eif().map(|i| Statement::If(i)),
            TokType::While => self.hwile().map(|w| Statement::While(w)),
            TokType::Break => Ok(Statement::Break(self._break())),
            TokType::Continue => Ok(Statement::Continue(self._continue())),
            TokType::Return => Ok(Statement::Return(self._return())),
            TokType::For => self._for(),
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
