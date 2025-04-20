use crate::mem::heap::Heap;
use crate::runtime::{Interpreter, builtin::BuiltIn};
use crate::parser::expr::Expr;
use crate::error::{Error, Special};
use crate::parser::expr::{Index, Call};
use crate::parser::statement::{Block, Proc};

#[derive(Clone, Debug)]
pub enum Val {
    Number(f64),
    Function(Vec<String>, Expr),
    BuiltIn(BuiltIn),
    Bool(bool),
    Unit,
    Proc(ProcVal),
    Str(u64),
    Arr(u64),
    Char(u8),
}

impl Val {
    pub fn to_string(&self, h: &Heap) -> String {
        use Val::*;
        match self {
            Number(n) => format!("{}", n),
            Function(_, e) => e.to_string(),
            BuiltIn(b) => b.to_string(),
            Unit => String::from("()"),
            Bool(b) => String::from(if *b { "true" } else { "false" }),
            // TODO print types
            Proc(_) => String::from("<procedure>"),
            Str(addr) => h.to_string(*addr),
            Arr(addr) => h.to_string(*addr),
            Char(c) => c.to_string(),
        }
    }
    pub fn type_as_string(&self) -> String {
        use Val::*;
        String::from(match self {
            Number(_) => "Number",
            Function(..) => "Function",
            BuiltIn(_) => "BuiltIn",
            Unit => "Unit",
            Bool(_) => "Bool",
            Proc(_) => "Procedure",
            Str(_) => "String",
            Arr(_) => "Array",
            Char(_) => "Char",
        })
    }
    fn gen_out_of_range_error(index: &Index, idx: usize, len: usize) -> Error {
        let msg = format!("Index out of bounds. Indexed location {} with object of length {}.", idx, len);
        Error { special: None,
            msg, line: index.lb.line, col_start: index.lb.col_start, col_end: index.rb.col_end
        }
    }
    pub fn index(&self, idx: usize, index: &Index, val: Option<Val>, h: &Heap) -> Result<Val, Error> {
        use Val::*;
        match self {
            Arr(addr) => {
                let len = h.len(*addr);
                if idx >= len {
                    return Err(Self::gen_out_of_range_error(index, idx, len));
                }
                return if let Some(val) = val {
                    h.set_arr(*addr, idx, val);
                    Ok(Val::Unit)
                } else {
                    Ok(h.get_at(*addr, idx).unwrap())
                };
            }
            Str(addr) => {
                if idx >= h.len(*addr) {
                    return Err(Self::gen_out_of_range_error(index, idx, h.len(*addr)));
                }
                return if let Some(val) = val {
                    match val {
                        Val::Char(c) => {
                            h.set_str(*addr, idx, c);
                            Ok(Val::Unit)
                        }
                        other => {
                            let msg = format!("Attempt to set string element to non-Char value of type {}.", other.type_as_string());
                            Err(Error { special: None,
                                msg, col_start: index.lb.col_start, col_end: index.rb.col_end, line: index.lb.line
                            })
                        }
                    }
                } else {
                    Ok(h.get_at(*addr, idx).unwrap())
                };
            }
            other => {
                let msg = format!("Attempt to index into a {}.", other.type_as_string());
                Err(Error { special: None,
                    msg, line: index.lb.line, col_end: index.rb.col_end, col_start: index.lb.col_start
                })
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct ProcVal {
    params: Vec<String>,
    body: Block,
}

impl ProcVal {
    pub fn from(p: Proc) -> Self {
        Self { params: p.params, body: p.body }
    }
    pub fn call(&self, c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() != self.params.len() {
            let msg = format!("Procedure expected {} arguments but received {}.", self.params.len(), c.args.len());
            return Err(Error {
                special: None, msg, col_start: c.identifier.col_start, col_end: c.rparen.col_end, line: c.identifier.line
            });
        }
        let scope = Interpreter::from(&i);
        for (k, arg) in c.args.iter().enumerate() {
            let val = i.expr(&arg)?;
            scope.env.put(self.params[k].clone(), val);
        }

        match scope.block(&self.body) {
            Ok(_) => Ok(Val::Unit),
            Err(Error { special: Some(Special::Return(e)), .. }) => {
                if let Some(e) = e {
                    Ok(scope.expr(&e)?)
                } else {
                    Ok(Val::Unit)
                }
            }
            Err(e) => Err(e),
        }
    }
}

