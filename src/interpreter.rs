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
    fn unwrap_val(v: Val, op: &Tok) -> Result<f64, Error> {
        match v {
            Val::Number(n) => Ok(n),
            Val::Function(..) => {
                let msg = format!("Attempt to '{}' with a function.", op.lexeme);
                Err(Error { msg, line: op.line, col_start: op.col_start, col_end: op.col_end })
            }
        }
    }
    fn binary(&self, b: Binary) -> Result<Val, Error> {
        use TokType::*;
        let mut result = Self::unwrap_val(self.expr(b.operands[0].clone())?, &b.ops[0])?;
        for i in 1..(b.operands.len()) {
            let v = Self::unwrap_val(self.expr(b.operands[i].clone())?, &b.ops[i - 1])?;
            match b.ops[0].t {
                Plus => result += v,
                Minus => result -= v,
                Star => result *= v,
                Slash => result = devide(result, v, &b.ops[i - 1])?,
                _ => unreachable!(),
            }
        }
        Ok(Val::Number(result))
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
    fn exp(&self, e: Exp) -> Result<Val, Error> {
        let base = self.expr(*e.base)?;
        let power = self.expr(*e.power)?;
        use Val::*;
        match (&base, &power) {
            (Number(a), Number(b)) => Ok(Val::Number(a.powf(*b))),
            _ => {
                let msg = format!("Attempt to raise a {} to the power of a {}. The exponent operator is only valid on numbers.",
                      base.kind_as_string(), power.kind_as_string()
                );
                Err(Error { msg, col_start: e.op.col_start, col_end: e.op.col_end, line: e.op.line })
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
            Expr::Exp(e) => self.exp(e),
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
