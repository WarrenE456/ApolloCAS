pub mod val;
pub mod builtin;

use crate::error::{Error, Special};
use crate::parser::statement::*;
use crate::scanner::tok::{Tok, TokType};
use crate::mem::heap::{Heap, HeapVal, HeapIter, Iter};
use crate::mem::env::Env;
use crate::runtime::val::{Val, FnVal};
use crate::parser::expr::*;
use crate::runtime::{builtin::BuiltIn, val::{Num, Type}};
use crate::sym::SymExpr;

use std::sync::Arc;

pub struct Interpreter {
    pub env: Arc<Env>,
    pub heap: Arc<Heap>,
}

impl<'a> Interpreter {
    pub fn new(heap: Arc<Heap>) -> Self {
        Self { env: Arc::new(Env::new()), heap }
    }
    pub fn from(other: &'a Interpreter) -> Self {
        Self { env: Env::from(Arc::clone(&other.env)), heap: Arc::clone(&other.heap) }
    }
    fn literal(&self, tok: &Tok) -> Result<Val, Error> {
        use TokType::*;
        match tok.t {
            Float => Ok(Val::Num(Num::Float(tok.lexeme.parse().unwrap()))),
            Int => Ok(Val::Num(Num::Int(tok.lexeme.parse().unwrap()))),
            True => Ok(Val::Bool(true)),
            False => Ok(Val::Bool(false)),
            Char => Ok(Val::Char(tok.lexeme.as_bytes()[0])),
            Unit => Ok(Val::Unit),
            Str => {
                let addr = self.heap.alloc(HeapVal::Str(Vec::from(tok.lexeme.as_bytes())));
                Ok(Val::Str(addr))
            }
            Identifier => self.env.get(&tok),
            _ => unreachable!(),
        }
    }
    fn binary(&self, b: &Binary) -> Result<Val, Error> {
        use TokType::*;
        let mut result = self.expr(&b.operands.first().unwrap())?;
        for (next, op) in std::iter::zip(b.operands[1..b.operands.len()].iter(), b.operators.iter()) {
            let next = self.expr(next)?;
            match (result, next) {
                (Val::Num(a), Val::Num(b)) => match op.t {
                    Plus => result = Val::Num(a + b),
                    Minus => result = Val::Num(a - b),
                    Star => result = Val::Num(a * b),
                    Slash => {
                        result = (a / b)
                            .map(|v| Val::Num(v))
                            .map_err(|msg| Error {
                            special: None, msg, line: op.line, col_end: op.col_end, col_start: op.col_start
                        })?;
                    }
                    _ => unreachable!(),
                }
                (Val::Sym(a), Val::Sym(b)) =>{
                    // TODO improve efficency
                    let a = match self.heap.get(a) {
                        Some(HeapVal::Sym(s)) => s,
                        _ => unreachable!(),
                    };
                    let b = match self.heap.get(b) {
                        Some(HeapVal::Sym(s)) => s,
                        _ => unreachable!(),
                    };
                    let expr = match op.t {
                        Plus => SymExpr::add(a, b),
                        Minus => SymExpr::add(a, Negate::negate_sym_expr(b)),
                        Star => SymExpr::mul(a, b),
                        Slash => todo!(),
                        _ => unreachable!(),
                    };
                    result = Val::Sym(self.heap.alloc(HeapVal::Sym(expr)));
                }
                (a, b) => {
                    let msg = format!(
                        "Cannot perform bianary operation '{}' on types {} and {}",
                        op.lexeme, a.type_as_string(&self.heap), b.type_as_string(&self.heap)
                    );
                    return Err(Error::from(msg, &op, &op));
                }
            }
        }
        Ok(result)
    }
    fn negate(&self, n: &Negate) -> Result<Val, Error> {
        match self.expr(&n.value)? {
            Val::Num(n) => {
                Ok(Val::Num(n * Num::Int(-1)))
            }
            a => Err(Error { special: None,
                msg: format!("Attempt to negate value of type {}.", a.type_as_string(&self.heap)),
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
            Val::Fn(p) => {
                self.heap.call(p, c,self)
            }
            other => {
                let col_start = c.identifier.col_start;
                let col_end = c.identifier.col_end;
                let line = c.identifier.line;
                return Err(Error { special: None,
                    msg: format!("Attempt to use function calling notation on a {}.",
                        other.type_as_string(&self.heap)), col_start, col_end, line
                });
            }
        }
    }
    fn exp(&self, e: &Exp) -> Result<Val, Error> {
        let base = self.expr(&e.base)?;
        let power = self.expr(&e.exp)?;
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
                      base.type_as_string(&self.heap), power.type_as_string(&self.heap)
                );
                Err(Error { special: None, msg, col_start: e.op.col_start, col_end: e.op.col_end, line: e.op.line })
            }
        }
    }
    fn comp(&self, c: &Comp) -> Result<Val, Error> {
        let mut prev = self.expr(&c.operands.first().unwrap())?;
        for (next, op) in std::iter::zip(c.operands[1..c.operands.len()].iter(), c.operators.iter()) {
            let next = self.expr(next)?;
            use TokType::*;
            let comp_result = match (prev, &next) {
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
                    Equal => a == *b,
                    BangEqual => a != *b,
                    _ => {
                        let msg = String::from("Attempt to do non-equality comparison with Bools.");
                        return Err(Error { special: None,
                            msg, line: op.line, col_start: op.col_start, col_end: op.col_end
                        });
                    }
                }
                (Val::Char(a), Val::Char(b)) => match op.t {
                    Equal => a == *b,
                    BangEqual => a != *b,
                    _ => {
                        let msg = String::from("Attempt to do non-equality comparison with Chars.");
                        return Err(Error { special: None,
                            msg, line: op.line, col_start: op.col_start, col_end: op.col_end
                        });
                    }
                }
                (Val::Unit, Val::Unit) => true,
                (Val::Str(s), Val::Str(ss)) => {
                    match (self.heap.get(s).unwrap(), self.heap.get(*ss).unwrap()) {
                        (HeapVal::Str(s), HeapVal::Str(ss)) => s == ss,
                        _ => unreachable!(),
                    }
                }
                (a, b) => {
                    if a.get_type(&self.heap) == b.get_type(&self.heap) {
                        let msg = format!(
                            "Attempt to compare values of type {}.",
                            a.type_as_string(&self.heap)
                        );
                        return Err(Error { special: None,
                            msg, line: op.line, col_start: op.col_start, col_end: op.col_end
                        });
                    } else {
                        return Ok(Val::Bool(false));
                    }
                }
            };
            if comp_result == false {
                return Ok(Val::Bool(false));
            }
            prev = next;
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
                    a.type_as_string(&self.heap), b.type_as_string(&self.heap),
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
                    l.type_as_string(&self.heap), r.type_as_string(&self.heap),
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
                let msg = format!("Attempted to use value of type {} as an index. Index must be an Int.", other.type_as_string(&self.heap));
                return Err(Error{ special: None,
                    msg, line: i.rb.line, col_start: i.lb.col_start, col_end: i.rb.col_end
                })
            },
        }
    }
    fn index(&self, i: &Index) -> Result<Val, Error> {
        self.expr(&i.expr)?.index(self.get_index(&i)?, &i, None, &self.heap)
    }
    fn concat(&self, c: &Concat) -> Result<Val, Error> {
        match (self.expr(&c.l)?, self.expr(&c.r)?) {
            (Val::Str(a), Val::Str(b))
            | (Val::Str(a), Val::Arr(b))
            | (Val::Arr(a), Val::Arr(b))
            | (Val::Arr(a), Val::Str(b)) => {
                match (self.heap.get(a), self.heap.get(b)) {
                    (Some(HeapVal::Str(a)), Some(HeapVal::Str(b))) => {
                        let mut a = a;
                        a.extend(b);
                        return Ok(Val::Str(self.heap.alloc(HeapVal::Str(a))));
                    }
                    (Some(HeapVal::Str(a)), Some(HeapVal::Arr(b))) => {
                        let mut a = a;
                        for val in b {
                            a.extend(val.to_string(&self.heap).as_bytes());
                        }
                        return Ok(Val::Str(self.heap.alloc(HeapVal::Str(a))));
                    }
                    (Some(HeapVal::Arr(a)), Some(HeapVal::Arr(b))) => {
                        let mut a = a;
                        a.extend(b);
                        return Ok(Val::Arr(self.heap.alloc(HeapVal::Arr(a))));
                    }
                    (Some(HeapVal::Arr(a)), Some(HeapVal::Str(b))) => {
                        let mut a = a;
                        let chars = b.iter().map(|c| Val::Char(*c)).collect::<Vec<_>>();
                        a.extend(chars);
                        return Ok(Val::Arr(self.heap.alloc(HeapVal::Arr(a))));
                    }
                    _ => unreachable!(),
                }
            }
            (a, b) => {
                let s = format!("{}{}", a.to_string(&self.heap), b.to_string(&self.heap))
                    .as_bytes().iter()
                    .cloned().collect::<Vec<_>>();
                return Ok(Val::Str(self.heap.alloc(HeapVal::Str(s))));
            }
        }
    }
    fn expr(&self, e: &Expr) -> Result<Val, Error> {
        use crate::parser::expr::Expr::*;
        return match e {
            Literal(tok) => self.literal(tok),
            Group(e) => self.expr(e),
            Binary(b) => self.binary(b),
            Concat(c) => self.concat(c),
            Negate(n) => self.negate(n),
            Call(c) => self.call(c),
            Exp(e) => self.exp(e),
            Comp(c) => self.comp(c),
            Or(o) => self.or(o),
            And(a) => self.and(a),
            Arr(a) => self.arr(a),
            Index(i) => self.index(i),
            Fn(f) => self._fn(f.clone()),
            Sym(dollar, e) => e.to_sym()
                .map(|se| {
                    Val::Sym(self.heap.alloc(HeapVal::Sym(se.simplify())))
                })
                .map_err(|msg| Error::from(msg, dollar, dollar)),
        };
    }
    pub fn eval_expr_at(&self, e: &Expr, var_name: &str, var: f64) -> Result<Val, Error> {
        let var = Val::Num(Num::Float(var));
        self.env.put(var_name.to_string(), var, &self.heap);
        self.expr(e)
    }
    fn var(&self, a: &Var) -> Result<(), Error> {
        let val = if let Type::Sym(t) = &a.t {
            let sym_expr = a.value
                .to_sym()
                .map_err(|s| Error::from(s, &a.op, &a.op))?.simplify();
            t.coerce(sym_expr)
                .map_err(|s| Error::from(s, &a.op, &a.op))
                .map(|symexpr| Val::Sym(self.heap.alloc(HeapVal::Sym(symexpr))))?
        } else {
            let val = self.expr(&a.value)?;
            a.t.coerce(val, &self.heap).map_err(|msg| {
                Error { special: None, msg,
                    col_start: a.op.col_start, col_end: a.op.col_end, line: a.op.line
                }
            })?
        };
        let t = if Type::Auto == a.t {
            val.get_type(&self.heap)
        } else {
            a.t.clone()
        };
        self.env.def(a.identifier.clone(), val.clone(), t)
    }
    fn set(&self, s: &Set) -> Result<(), Error> {
        let val = self.expr(&s.value)?;
        self.env.set(s.identifier.clone(), val.clone(), &self.heap)
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
                let msg = format!("Attempt to use a {} as the condition of an if statement, expected Bool.", a.type_as_string(&self.heap));
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
                    let msg = format!(
                        "Attempt to use a {} as the condition of an while statement, expected Bool.",
                        a.type_as_string(&self.heap)
                    );
                    return Err(Error { special: None,
                        msg, col_start: w.hwile.col_start, col_end: w.hwile.col_end, line: w.hwile.line
                    })
                }
            }
        }
            Ok(())
    }
    fn _fn(&self, p: Fn) -> Result<Val, Error> {
        let addr = self.heap.alloc(HeapVal::Fn(Arc::new(FnVal::from(p))));
        Ok(Val::Fn(addr))
    }
    fn set_index(&self, s: &SetIndex) -> Result<(), Error> {
        self.expr(&s.index.expr)?
            .index(self.get_index(&s.index)?, &s.index.clone(), Some(self.expr(&s.value)?), &self.heap)?;
        Ok(())
    }
    fn to_iter(addr: u64, h: &Heap) -> u64 {
        let iter = HeapVal::Iter(Iter::Heap(HeapIter::new(addr)));
        h.alloc(iter)
    }
    fn _for(&self, f: &For) -> Result<(), Error> {
        let addr = match self.expr(&f.iter)? {
            Val::Arr(addr) | Val::Str(addr) => Self::to_iter(addr, &self.heap),
            Val::Iter(i) => i,
            other => {
                let msg = format!("Attempt to iterate over a value of type {}.", other.type_as_string(&self.heap));
                return Err(Error { special: None, msg,
                    col_start: f.fro.col_start, col_end: f.fro.col_end, line: f.fro.line
                });
            }
        };
        self.heap.add_pin(addr);
        while let Some(v) = self.heap.next_iter(addr) {
            let for_scope = Interpreter::from(self);
            for_scope.env.put(f.identifier.lexeme.clone(), v, &self.heap);
            for_scope.block(&f.body)?;
        }
        self.heap.rm_pin(addr);  
        Ok(())
    }
    pub fn interpret(&'a self, stmt: &Statement) -> Result<Option<Val>, Error> {
        use Statement::*;
        return match stmt {
            Expr(e) => self.expr(e).map(|e| Some(e)),
            Var(a) => {self.var(a)?; Ok(None)}
            Set(s) => {self.set(s)?; Ok(None)}
            SetIndex(s) => {self.set_index(s)?; Ok(None)}
            If(i) => {self.eif(i)?; Ok(None)}
            Block(b) => {
                let inner_scope = Self::from(self);
                inner_scope.block(b)?;
                Ok(None)
            },
            While(w) => {self.hwile(w)?; Ok(None)}
            For(f) => {self._for(f)?; Ok(None)}
            Break(e) => Err(e.clone()),
            Continue(e) => Err(e.clone()),
            Return(e) => Err(e.clone()),
        };
    }
}
