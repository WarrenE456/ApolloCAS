use crate::error::Error;
use crate::line::{Line, Expr, Binary};
use crate::scanner::{Tok, TokType};
use std::fmt;

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

pub struct Interpreter {
}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }
    fn literal(&self, tok: Tok) -> Val {
        use TokType::*;
        match tok.t {
            Number => Val::Number(tok.lexeme_as_str().parse().unwrap()),
            Identifier => todo!(),
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
    fn expr(&self, e: Expr) -> Result<Val, Error> {
        return match e {
            Expr::Binary(b) => self.binary(b),
            Expr::Literal(tok) => Ok(self.literal(tok)),
        };
    }
    pub fn interpret(&self, line: Line) -> Result<Val, Error> {
        use Line::*;
        return match line {
            Expr(e) => self.expr(e),
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
