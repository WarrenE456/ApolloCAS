pub mod val;
pub mod builtin;

use crate::error::{Error, Special};
use crate::parser::statement::*;
use crate::scanner::tok::{Tok, TokType};
use crate::mem::env::Env;
use crate::mem::heap::{Heap, HeapVal, HeapIter};
use crate::graph::GraphSignal;
use crate::runtime::val::{Val, ProcVal};
use crate::parser::expr::*;
use crate::runtime::{builtin::BuiltIn, val::Num};

use std::sync::mpsc::Sender;
use std::sync::Arc;

pub struct Interpreter {
    graph_tx: Sender<GraphSignal>,
    pub env: Arc<Env>,
    pub heap: Arc<Heap>,
}

impl<'a> Interpreter {
    pub fn new(graph_tx: Sender<GraphSignal>, heap: Arc<Heap>) -> Self {
        Self { env: Arc::new(Env::new()), graph_tx, heap }
    }
    pub fn from(other: &'a Interpreter) -> Self {
        Self { env: Env::from(Arc::clone(&other.env)), graph_tx: other.graph_tx.clone(), heap: Arc::clone(&other.heap) }
    }
    fn literal(&self, tok: &Tok) -> Result<Val, Error> {
        use TokType::*;
        match tok.t {
            Float => Ok(Val::Num(Num::Float(tok.lexeme.parse().unwrap()))),
            Int => Ok(Val::Num(Num::Int(tok.lexeme.parse().unwrap()))),
            True => Ok(Val::Bool(true)),
            False => Ok(Val::Bool(false)),
            Str => {
                let addr = self.heap.alloc(HeapVal::Str(Vec::from(tok.lexeme.as_bytes())));
                Ok(Val::Str(addr))
            }
            Identifier => self.env.get(&tok),
            _ => unreachable!(),
        }
    }
    fn expect_number(&self, e: &Expr, line: usize, col_start: usize, col_end: usize) -> Result<Num, Error> {
        match self.expr(e)? {
            Val::Num(n) => Ok(n),
            other => {
                let msg = format!("Expected Number but found {}.", other.type_as_string());
                Err(Error { special: None, msg, line, col_start, col_end })
            }
        }
    }
    fn binary(&self, b: &Binary) -> Result<Val, Error> {
        use TokType::*;
        let op = b.operators.first().unwrap();
        let mut result = self.expect_number(&b.operands.first().unwrap(), op.line, op.col_start, op.col_end)?;
        for (next, op) in std::iter::zip(b.operands[1..b.operands.len()].iter(), b.operators.iter()) {
            let next = self.expect_number(next, op.line, op.col_start, op.col_end)?;
            match op.t {
                Plus => result = result + next,
                Minus => result = result - next,
                Star => result = result * next,
                Slash => {
                    result = (result / next).map_err(|msg| Error {
                        special: None, msg, line: op.line, col_end: op.col_end, col_start: op.col_start
                    })?;
                }
                _ => unreachable!(),
            }
        }
        Ok(Val::Num(result))
    }
    fn negate(&self, n: &Negate) -> Result<Val, Error> {
        match self.expr(&n.value)? {
            Val::Num(n) => {
                Ok(Val::Num(n * Num::Int(-1)))
            }
            a => Err(Error { special: None,
                msg: format!("Attempt to negate value of type {}.", a.type_as_string()),
                line: n.minus.line,
                col_end: n.minus.col_end,
                col_start: n.minus.col_start
            }),
        }
    }
    fn call(&self, c: &Call) -> Result<Val, Error> {

        if let Some(built_in) = BuiltIn::is_builtin(&c.identifier.lexeme) {
            return built_in.call(c, self);
        }

        let f = self.env.get(&c.identifier)?;
        match f {
            Val::Proc(p) => {
                p.call(c, self)
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
                        Error { special: None, msg, col_start, col_end, line }
                    );
                }
                let scope = Interpreter::from(self);
                for (i, arg) in c.args.iter().enumerate() {
                    scope.env.put(params[i].clone(), self.expr(arg)?);
                }
                scope.expr(&body)
            }
            other => {
                let col_start = c.identifier.col_start;
                let col_end = c.identifier.col_end;
                let line = c.identifier.line;
                return Err(Error { special: None,
                    msg: format!("Attempt to use function calling notation on a {}.", other.type_as_string()), col_start, col_end, line
                });
            }
        }
    }
    fn exp(&self, e: &Exp) -> Result<Val, Error> {
        let base = self.expr(&e.base)?;
        let power = self.expr(&e.power)?;
        match (&base, &power) {
            (Val::Num(a), Val::Num(b)) => Ok(Val::Num(match (a, b) {
                (Num::Int(a), Num::Int(b)) => {
                    if *b >= 0 {
                        Num::Int(a.pow(*b as u32))
                    } else {
                        Num::Float((*a as f64).powf(*b as f64))
                    }
                }
                (a, b) => Num::Float(a.to_float().powf(b.to_float())),
            })),
            _ => {
                let msg = format!("Attempt to raise a {} to the power of a {}. The exponent operator is only valid on numbers.",
                      base.type_as_string(), power.type_as_string()
                );
                Err(Error { special: None, msg, col_start: e.op.col_start, col_end: e.op.col_end, line: e.op.line })
            }
        }
    }
    fn comp(&self, c: &Comp) -> Result<Val, Error> {
        let vals = vec![self.expr(&c.operands.first().unwrap())?];
        for (next, op) in std::iter::zip(c.operands[1..c.operands.len()].iter(), c.operators.iter()) {
            let val = self.expr(next)?;
            use TokType::*;
            let val = match (vals.last().unwrap(), &val) {
                (Val::Num(a), Val::Num(b)) => {
                    let a = a.to_float();
                    let b = b.to_float();
                    match op.t {
                        Greater => a > b,
                        GreaterEqual => a >= b,
                        Lesser => a < b,
                        LesserEqual => a <= b,
                        Equal => a == b,
                        BangEqual => a != b,
                        _ => unreachable!(),
                    }
                }
                (Val::Bool(a), Val::Bool(b)) => match op.t {
                    Equal => a == b,
                    BangEqual => a != b,
                    _ => {
                        let msg = String::from("Attempt to do non-equality comparison with Bools.");
                        return Err(Error { special: None,
                            msg, line: op.line, col_start: op.col_start, col_end: op.col_end
                        });
                    }
                }
                (a, b) => {
                    let msg = format!(
                        "Attempt to compare types {} and {}.",
                        a.type_as_string(), b.type_as_string()
                    );
                    return Err(Error { special: None,
                        msg, line: op.line, col_start: op.col_start, col_end: op.col_end
                    });
                }
            };
            if val == false {
                return Ok(Val::Bool(false));
            }
        }
        Ok(Val::Bool(true))
    }
    fn or(&self, o: &Or) -> Result<Val, Error> {
        match (self.expr(&o.left)?, self.expr(&o.right)?) {
            (Val::Bool(a), Val::Bool(b)) => {
                Ok(Val::Bool(a || b))
            }
            (a, b) => {
                let msg = format!(
                    "Attempt to 'or' types {} and {}. This can only be done with Bools.",
                    a.type_as_string(), b.type_as_string(),
                );
                return Err(Error { special: None,
                    msg, line: o.op.line, col_start: o.op.col_start, col_end: o.op.col_end,
                });
            }
        }
    }
    fn and(&self, a: &And) -> Result<Val, Error> {
        match (self.expr(&a.left)?, self.expr(&a.right)?) {
            (Val::Bool(l), Val::Bool(r)) => {
                Ok(Val::Bool(l && r))
            }
            (l, r) => {
                let msg = format!(
                    "Attempt to 'or' types {} and {}. This can only be done with Bools.",
                    l.type_as_string(), r.type_as_string(),
                );
                return Err(Error { special: None,
                    msg, line: a.op.line, col_start: a.op.col_start, col_end: a.op.col_end
                });
            }
        }
    }
    fn arr(&self, elements: &Vec<Expr>) -> Result<Val, Error> {
        let mut vals = Vec::new();
        for element in elements {
            vals.push(self.expr(element)?);
        }
        let addr = self.heap.alloc(HeapVal::Arr(vals));
        Ok(Val::Arr(addr))
    }
    fn get_index(&self, i: &Index) -> Result<usize, Error> {
        match self.expr(&i.index)? {
            Val::Num(Num::Int(n)) => Ok(n as usize),
            other => {
                let msg = format!("Attempted to use value of type {} as an index. Index must be an Int.", other.type_as_string());
                return Err(Error{ special: None,
                    msg, line: i.rb.line, col_start: i.lb.col_start, col_end: i.rb.col_end
                })
            },
        }
    }
    fn index(&self, i: &Index) -> Result<Val, Error> {
        self.expr(&i.expr)?.index(self.get_index(&i)?, &i, None, &self.heap)
    }
    fn expr(&self, e: &Expr) -> Result<Val, Error> {
        use crate::parser::expr::Expr::*;
        return match e {
            Literal(tok) => self.literal(tok),
            Group(e) => self.expr(e),
            Binary(b) => self.binary(b),
            Negate(n) => self.negate(n),
            Call(c) => self.call(c),
            Exp(e) => self.exp(e),
            Comp(c) => self.comp(c),
            Or(o) => self.or(o),
            And(a) => self.and(a),
            Arr(a) => self.arr(a),
            Index(i) => self.index(i),
        };
    }
    pub fn eval_expr_at(&self, e: &Expr, var_name: &str, var: f64) -> Result<Val, Error> {
        let var = Val::Num(Num::Float(var));
        self.env.put(var_name.to_string(), var);
        self.expr(e)
    }
    fn var(&self, a: &Var) -> Result<(), Error> {
        let val = a.t.coerce(self.expr(&a.value)?).map_err(|msg| {
            Error { special: None, msg,
                col_start: a.op.col_start, col_end: a.op.col_end, line: a.op.line
            }
        })?;
        self.env.def(a.identifier.clone(), val.clone())
    }
    fn set(&self, s: &Set) -> Result<(), Error> {
        let val = self.expr(&s.value)?;
        self.env.set(s.identifier.clone(), val.clone())
    }
    fn def(&self, d: &Def) -> Result<(), Error> {
        self.env.def(
            d.identifier.clone(),
            Val::Function(d.args.clone(), d.value.clone()),
        )?;
        Ok(())
    }
    fn block(&self, b: &Block) -> Result<(), Error> {
        for statement in b.statements.iter() {
            self.interpret(statement)?;
        }
        Ok(())
    }
    fn eif(&self, i: &If) -> Result<(), Error> {
        match self.expr(&i.cond)? {
            Val::Bool(b) => {
                if b {
                    self.interpret(&i.if_branch)?;
                } else {
                    if let Some(else_branch) = &i.else_branch {
                        self.interpret(&else_branch)?;
                    }
                }
            }
            a => {
                let msg = format!("Attempt to use a {} as the condition of an if statement, expected Bool.", a.type_as_string());
                return Err(Error { special: None,
                    msg, col_start: i.eif.col_start, col_end: i.eif.col_end, line: i.eif.line
                })
            }
        }
        Ok(())
    }
    fn hwile(&self, w: &While) -> Result<(), Error> {
        'a: loop {
            match self.expr(&w.cond)? {
                Val::Bool(b) => {
                    if b {
                        match self.interpret(&w.body) {
                            Err(e) => if let Some(s) = e.special.clone() {
                                use Special::*;
                                match s {
                                    Break => { break 'a },
                                    Continue => continue,
                                    _ => { return Err(e); }
                                }
                            } else {
                                return Err(e);
                            }
                            Ok(_) => continue,
                        }
                    } else {
                        break;
                    }
                }
                a => {
                    let msg = format!("Attempt to use a {} as the condition of an while statement, expected Bool.", a.type_as_string());
                    return Err(Error { special: None,
                        msg, col_start: w.hwile.col_start, col_end: w.hwile.col_end, line: w.hwile.line
                    })
                }
            }
        }
            Ok(())
    }
    fn proc(&self, p: Proc) -> Result<(), Error> {
        self.env.def(p.name.clone(), Val::Proc(ProcVal::from(p)))
    }
    fn set_index(&self, s: &SetIndex) -> Result<(), Error> {
        self.expr(&s.index.expr)?
            .index(self.get_index(&s.index)?, &s.index.clone(), Some(self.expr(&s.value)?), &self.heap)?;
        Ok(())
    }
    fn fro(&self, f: &For) -> Result<(), Error> {
        let addr = match self.expr(&f.iter)? {
            Val::Arr(addr) => addr,
            Val::Str(addr) => addr,
            other => {
                let msg = format!("Attempt to iterate over a {}.", other.type_as_string());
                return Err(Error { special: None, msg,
                    col_start: f.fro.col_start, col_end: f.fro.col_end, line: f.fro.line
                });
            }
        };
        let mut iter = HeapIter::new(addr);
        let inner_scope = Interpreter::from(self);
        while let Some(v) = iter.next(&self.heap) {
            inner_scope.env.put(f.identifier.lexeme.clone(), v);
            inner_scope.block(&f.body)?;
        }
        Ok(())
    }
    pub fn interpret(&'a self, stmt: &Statement) -> Result<Option<Val>, Error> {
        use Statement::*;
        return match stmt {
            Expr(e) => self.expr(e).map(|e| Some(e)),
            Var(a) => {self.var(a)?; Ok(None)}
            Set(s) => {self.set(s)?; Ok(None)}
            SetIndex(s) => {self.set_index(s)?; Ok(None)}
            Def(d) => {self.def(d)?; Ok(None)}
            If(i) => {self.eif(i)?; Ok(None)}
            Block(b) => {
                let inner_scope = Self::from(self);
                inner_scope.block(b)?;
                Ok(None)
            },
            While(w) => {self.hwile(w)?; Ok(None)}
            Proc(p) => {self.proc(p.clone())?; Ok(None)}
            For(f) => {self.fro(f)?; Ok(None)}
            Break(e) => Err(e.clone()),
            Continue(e) => Err(e.clone()),
            Return(e) => Err(e.clone()),
            Command(_) => todo!(),
        };
    }
}
