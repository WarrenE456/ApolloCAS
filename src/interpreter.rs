use std::fmt;

use crate::error::Error;
use crate::statement::*;
use crate::scanner::{Tok, TokType};
use crate::environment::Env;

#[derive(Clone)]
pub enum Val {
    Number(f64),
}

impl Val {
    pub fn as_string(&self) -> String {
        match self {
            Self::Number(n) => format!("{}", n),
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

pub struct Interpreter<'a> {
    env: Env<'a>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self { env: Env::new() }
    }
    fn literal(&self, tok: Tok) -> Result<Val, Error> {
        use TokType::*;
        match tok.t {
            Number => Ok(Val::Number(tok.lexeme_as_str().parse().unwrap())),
            Identifier => self.env.get(&tok),
            _ => unreachable!(),
        }
    }
    fn binary(&self, b: Binary) -> Result<Val, Error> {
        let l = self.expr(*b.l)?;
        let r = self.expr(*b.r)?;
        use TokType::*;
        Ok(match (l, r) {
            (Val::Number(x), Val::Number(y)) => Val::Number(match b.op.t {
                Plus => x + y,
                Minus => x - y,
                Star => x * y,
                Slash => devide(x, y, &b.op)?,
                _ => unreachable!(),
            })
        })
    }
    fn assignment(&self, a: Assignment) -> Result<Val, Error> {
        let val = self.expr(a.value)?;
        self.env.def(a.identifier.lexeme_as_str().to_owned(), val.clone());
        Ok(val)
    }
    fn expr(&self, e: Expr) -> Result<Val, Error> {
        return match e {
            Expr::Literal(tok) => self.literal(tok),
            Expr::Binary(b) => self.binary(b),
        };
    }
    pub fn interpret(&self, line: Statement) -> Result<Val, Error> {
        use Statement::*;
        return match line {
            Expr(e) => self.expr(e),
            Assignment(a) => self.assignment(a),
            Command(_) => todo!(),
        };
    }
}

fn devide(x: f64, y: f64, op: &Tok) -> Result<f64, Error> {
    return if y == 0.0 {
        let msg = String::from("Attempt to devide by zero.");
        Err(Error { msg, line: op.line, col_start: op.col_start, col_end: op.col_end })
    } else {
        Ok(x / y)
    };
}
