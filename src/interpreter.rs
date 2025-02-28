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
            Self::Function(_, e) => e.to_string(),
        }
    }
    pub fn kind_as_string(&self) -> String {
        match self {
            Self::Number(_) => String::from("Number"),
            Self::Function(..) => String::from("Function"),
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
    pub fn from(other: &'a Interpreter) -> Self {
        Self { env: Env::from(&other.env) }
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
            (x, y) => {
                let msg = format!("Attempt to use '{}' operator on a {} and {}.", 
                    b.op.lexeme, x.kind_as_string(), y.kind_as_string());
                return Err(Error { msg, line: b.op.line, col_start: b.op.col_start, col_end: b.op.col_end });
            }
        })
    }
    fn negate(&self, n: Negate) -> Result<Val, Error> {
        match self.expr(*n.value)? {
            Val::Number(n) => {
                Ok(Val::Number(-1.0 * n))
            }
            Val::Function(..) => Err(Error {
                msg: String::from("Attempt to negate a function."),
                line: n.minus.line,
                col_end: n.minus.col_end,
                col_start: n.minus.col_start
            }),
        }
    }
    fn call(&self, c: Call) -> Result<Val, Error> {
        let f = self.env.get(&c.identifier)?;
        match f {
            Val::Number(_) => {
                let col_start = c.identifier.col_start;
                let col_end = c.identifier.col_end;
                let line = c.identifier.line;
                return Err(
                    Error { msg: format!("Attempt to use function calling notation on a number."), col_start, col_end, line }
                );
            }
            Val::Function(params, body) => {
                if c.args.len() != params.len() {
                    let col_start = c.identifier.col_start;
                    let col_end = c.identifier.col_end;
                    let line = c.identifier.line;
                    let msg = format!(
                        "Attempted to call function '{}' that takes {} arguments with {}.",
                        c.identifier.lexeme, params.len(), c.args.len()
                    );
                    return Err(
                        Error { msg, col_start, col_end, line }
                    );
                }
                let scope = Interpreter::from(self);
                for (i, arg) in c.args.into_iter().enumerate() {
                    scope.env.def(params[i].clone(), self.expr(arg)?);
                }
                scope.expr(body)
            }
        }
    }
    fn expr(&self, e: Expr) -> Result<Val, Error> {
        return match e {
            Expr::Literal(tok) => self.literal(tok),
            Expr::Group(e) => self.expr(*e),
            Expr::Binary(b) => self.binary(b),
            Expr::Negate(n) => self.negate(n),
            Expr::Call(c) => self.call(c),
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
