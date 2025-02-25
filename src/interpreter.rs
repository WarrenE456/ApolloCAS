use std::fmt;

use crate::error::Error;
use crate::statement::*;
use crate::scanner::{Tok, TokType};
use crate::environment::Env;

#[derive(Clone)]
pub enum Val {
    Number(f64),
    Function(Vec<String>, Expr),
}

impl Val {
    pub fn as_string(&self) -> String {
        match self {
            Self::Number(n) => format!("{}", n),
            Self::Function(..) => todo!(),
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
            Number => Ok(Val::Number(tok.lexeme.parse().unwrap())),
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
                // TODO optimize
                Carrot => x.powf(y),
                _ => unreachable!(),
            }),
            _ => todo!(),
        })
    }
    fn negate(&self, n: Negate) -> Result<Val, Error> {
        match self.expr(*n.value)? {
            Val::Number(n) => {
                Ok(Val::Number(-1.0 * n))
            }
            _ => todo!()
        }
    }
    fn expr(&self, e: Expr) -> Result<Val, Error> {
        return match e {
            Expr::Literal(tok) => self.literal(tok),
            Expr::Binary(b) => self.binary(b),
            Expr::Negate(n) => self.negate(n),
        };
    }
    fn var(&self, a: Var) -> Result<(), Error> {
        let val = self.expr(a.value)?;
        self.env.def(a.identifier.lexeme, val.clone());
        Ok(())
    }
    fn def(&self, d: Def) -> Result<(), Error> {
        self.env.def(
            d.identifier.lexeme,
            Val::Function(d.args, d.value),
        );
        Ok(())
    }
    pub fn interpret(&self, stmt: Statement) -> Result<Option<Val>, Error> {
        use Statement::*;
        return match stmt {
            Expr(e) => self.expr(e).map(|e| Some(e)),
            Var(a) => {self.var(a)?; Ok(None)},
            Def(d) => {self.def(d)?; Ok(None)},
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
