use std::fmt;

use crate::error::Error;
use crate::statement::*;
use crate::scanner::{Tok, TokType};
use crate::environment::Env;

#[derive(Clone)]
pub enum Val {
    Number(f64),
    Function(Vec<String>, Expr),
    BuiltIn(BuiltIn),
}

#[derive(Clone, Copy, Debug)]
enum BuiltInT {
    Log,
}

#[derive(Clone)]
pub struct BuiltIn {
    t: BuiltInT
}

impl BuiltIn {
    fn is_builtin(s: &str) -> Option<Self> {
        use BuiltInT::*;
        let t = match s {
            "log" => Log,
            _ => return None,
        };
        Some(BuiltIn { t })
    }
    fn gen_error(msg: String, c: Call) -> Error {
        Error {
            msg,
            line: c.identifier.line,
            col_start: c.identifier.col_start,
            col_end: c.rparen.col_end,
        }
    }
    fn log(c: Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() == 2 {
            let base = i.expr(c.args[0].clone())?;
            let arg2 = i.expr(c.args[1].clone())?;
            match (base, arg2) {
                (Val::Number(base), Val::Number(x)) => {
                    Ok(Val::Number(x.log(base)))
                },
                (a, b) => {
                    let msg = format!(
                        "Attempt take the log of a {} with the base of a {}. Both should be Numbers.",
                        a.type_as_string(), b.type_as_string(), 
                    );
                    Err(Self::gen_error(msg, c))
                }
            }
        }
        else if c.args.len() == 1 {
            let a = i.expr(c.args[0].clone())?;
            match a {
                Val::Number(a) => Ok(Val::Number(a.log10())),
                _ => {
                    Err(Self::gen_error(format!("Can't take the log of a {}.", a.type_as_string()), c))
                },
            }
        }
        else {
            Err(Self::gen_error(String::from("Log takes one or two arguments."), c))
        }
    }
    fn call(&self, c: Call, i: &Interpreter) -> Result<Val, Error> {
        use BuiltInT::*;
        match self.t {
            Log => {
                Self::log(c, i)
            },
        }
    }
    pub fn to_string(&self) -> String {
        use BuiltInT::*;
        String::from(match self.t {
            Log => "log",
        })
    }
}

impl Val {
    pub fn as_string(&self) -> String {
        match self {
            Self::Number(n) => format!("{}", n),
            Self::Function(_, e) => e.to_string(),
            Self::BuiltIn(b) => b.to_string(),
        }
    }
    pub fn type_as_string(&self) -> String {
        String::from(match self {
            Self::Number(_) => "Number",
            Self::Function(..) => "Function",
            Self::BuiltIn(_) => "BuiltIn",
        })
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
    fn expect_number(&self, e: Expr, line: usize, col_start: usize, col_end: usize) -> Result<f64, Error> {
        match self.expr(e)? {
            Val::Number(n) => Ok(n),
            other => {
                let msg = format!("Expected Number but found {}.", other.type_as_string());
                Err(Error { msg, line, col_start, col_end })
            }
        }
    }
    fn binary(&self, b: Binary) -> Result<Val, Error> {
        let mut b = b;
        use TokType::*;
        let mut op = b.operators.pop().unwrap();
        let mut result = self.expect_number(b.operands.pop().unwrap(), op.line, op.col_start, op.col_end)?;
        loop {
            let next = self.expect_number(b.operands.pop().unwrap(), op.line, op.col_start, op.col_end)?;
            match op.t {
                Plus => result += next,
                Minus => result -= next,
                Star => result *= next,
                Slash => {
                    if next == 0.0 {
                        let msg = String::from("Attempt to devide by 0.");
                        return Err(
                            Error { msg, line: op.line, col_end: op.col_end, col_start: op.col_start }
                        );
                    } else {
                        result /= next;
                    }
                }
                _ => unreachable!(),
            }
            op = match b.operators.pop() {
                Some(op) => op,
                None => break,
            }
        }
        Ok(Val::Number(result))
    }
    fn negate(&self, n: Negate) -> Result<Val, Error> {
        match self.expr(*n.value)? {
            Val::Number(n) => {
                Ok(Val::Number(-1.0 * n))
            }
            a => Err(Error {
                msg: format!("Attempt to negate value of type {}.", a.type_as_string()),
                line: n.minus.line,
                col_end: n.minus.col_end,
                col_start: n.minus.col_start
            }),
        }
    }
    fn call(&self, c: Call) -> Result<Val, Error> {

        if let Some(built_in) = BuiltIn::is_builtin(&c.identifier.lexeme) {
            return built_in.call(c, self);
        }

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
                    let col_end = c.rparen.col_end;
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
            Val::BuiltIn(_) => unreachable!(),
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
                      base.type_as_string(), power.type_as_string()
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
