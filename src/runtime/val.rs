use std::ops::{Mul, Div, Add, Sub};

use crate::mem::heap::Heap;
use crate::runtime::{Interpreter, builtin::BuiltIn};
use crate::parser::expr::Expr;
use crate::error::{Error, Special};
use crate::parser::expr::{Index, Call};
use crate::parser::statement::{Block, Proc};
use crate::scanner::tok::Tok;

#[derive(Clone, Debug, Copy)]
pub enum Num {
    Float(f64),
    Int(i64),
}

impl Mul for Num {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        match (self, other) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a * b),
            (a, b) => Num::Float(a.to_float() * b.to_float()) 
        }
    }
}

impl Div for Num {
    type Output = Result<Self, String>;
    fn div(self, other: Self) -> Result<Self, String> {
        let (a, b) = (self.to_float(), other.to_float());
        if b == 0.0 {
            Err(String::from("Attempt to devide by 0."))
        } else {
            Ok(Num::Float(a / b))
        }
    }
}

impl Add for Num {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        match (self, other) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a + b),
            (a, b) => Num::Float(a.to_float() + b.to_float()) 
        }
    }
}

impl Sub for Num {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        match (self, other) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a - b),
            (a, b) => Num::Float(a.to_float() - b.to_float()) 
        }
    }
}


impl Num {
    pub fn to_string(&self) -> String {
        use Num::*;
        match self {
            Int(n) => n.to_string(),
            Float(n) => n.to_string(),
        }
    }
    pub fn to_float(self) -> f64 {
        use Num::*;
        match self {
            Int(n) => n as f64,
            Float(n) => n,
        }
    }
    pub fn to_int(self) -> i64 {
        use Num::*;
        match self {
            Int(n) => n,
            Float(n) => n as i64,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Val {
    Num(Num),
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
            Num(n) => n.to_string(),
            Function(_, e) => e.to_string(),
            BuiltIn(b) => b.to_string(),
            Unit => String::from("()"),
            Bool(b) => String::from(if *b { "true" } else { "false" }),
            // TODO print types
            Proc(_) => String::from("<procedure>"),
            Str(addr) => h.to_string(*addr),
            Arr(addr) => h.to_string(*addr),
            Char(c) => (*c as char).to_string(),
        }
    }
    pub fn get_type(&self) -> Type {
        match self {
            Val::Num(Num::Int(_)) => Type::Int,
            Val::Num(Num::Float(_)) => Type::Float,
            Val::Function(..) => Type::Fn,
            Val::BuiltIn(_) => Type::BuiltIn,
            Val::Unit => Type::Unit,
            Val::Bool(_) => Type::Bool,
            Val::Proc(ProcVal { params, return_t, ..}) => {
                let param_t: Vec<Type> = params.iter().map(|(_, t)| t.clone()).collect();
                Type::Proc(param_t, Box::new(return_t.clone()))
            }
            Val::Str(_) => Type::Str,
            Val::Arr(_) => Type::Arr,
            Val::Char(_) => Type::Char,
        }
    }
    // TODO remove
    pub fn type_as_string(&self) -> String {
        self.get_type().to_string()
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


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Any,
    Int,
    Float,
    Fn,
    BuiltIn,
    Bool,
    Unit,
    Str,
    Arr,
    Char,
    Auto,
    Proc(Vec<Type>, Box<Type>),
}

impl Type {
    fn to_string(&self) -> String {
        use Type::*;
        match self {
            Any => String::from("Any"),
            Int => String::from("Int"),
            Float => String::from("Float"),
            Fn => String::from("Fn"),
            BuiltIn => String::from("BuiltIn"),
            Bool => String::from("Bool"),
            Unit => String::from("Unit"),
            Str => String::from("Str"),
            Arr => String::from("Arr"),
            Char => String::from("Char"),
            Auto => String::from("Auto"),
            Proc(param, ret) =>
                format!("({} -> {})", param.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "), ret.to_string()),
        }
    }
    fn gen_type_error(expected: &Type, found: &Type) -> String {
        format!("Expected type {} but found {}.", expected.to_string(), found.to_string())
    }
    pub fn coerce(&self, v: Val) -> Result<Val, String> {
        match self {
            Self::Any | Self::Auto => Ok(v),
            Self::Float => match v {
                Val::Num(Num::Float(_)) => Ok(v),
                Val::Num(Num::Int(n)) => Ok(Val::Num(Num::Float(n as f64))),
                other => Err(Self::gen_type_error(&Type::Float, &other.get_type()))
            }
            _ => {
                let other = v.get_type();
                if other == *self {
                    Ok(v)
                } else {
                    Err(Self::gen_type_error(self, &other))
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct ProcVal {
    params: Vec<(Tok, Type)>,
    return_t: Type,
    body: Block,
}

impl ProcVal {
    pub fn from(p: Proc) -> Self {
        Self { params: p.params, return_t: p.return_t, body: p.body }
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
            let (identifier, t) = &self.params[k];
            let val = t.coerce(val).map_err(|msg| {
                let msg = format!("{} (at argument {})", msg, k + 1);
                Error{ special: None,
                    msg, col_start: c.identifier.col_start, col_end: c.rparen.col_end, line: c.identifier.line 
                }
            })?;
            scope.env.put(identifier.lexeme.clone(), val);
        }

        match scope.block(&self.body) {
            Ok(_) => Ok(Val::Unit),
            Err(Error { special: Some(Special::Return(e, r)), .. }) => {
                if let Some(e) = e {
                    let return_val = self.return_t.coerce(scope.expr(&e)?).map_err(|msg| {
                        Error { special: None,
                            msg, col_start: r.col_start, col_end: r.col_end, line: r.line
                        }
                    })?;
                    Ok(return_val)
                } else {
                    Ok(Val::Unit)
                }
            }
            Err(e) => Err(e),
        }
    }
}

