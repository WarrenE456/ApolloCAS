use std::ops::{Mul, Div, Add, Sub};

use crate::mem::heap::{Heap, HeapVal};
use crate::runtime::{Interpreter, builtin::BuiltIn};
use crate::error::{Error, Special};
use crate::parser::expr::{Index, Call};
use crate::parser::statement::{Block, Fn};
use crate::scanner::tok::Tok;
use crate::sym::*;

use num_bigint::ToBigInt;

#[derive(Clone, Debug, Copy)]
pub enum Num {
    Float(f64),
    Int(i64),
}

impl Num {
    fn to_sym(&self) -> SymExpr {
        match self {
            Num::Int(i) => SymExpr::Z(i.to_bigint().unwrap()),
            Num::Float(_) => todo!()
        }
    }
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
    BuiltIn(BuiltIn),
    Bool(bool),
    Unit,
    Fn(u64),
    Str(u64),
    Arr(u64),
    Char(u8),
    Iter(u64),
    Sym(u64),
}

impl Val {
    pub fn to_string(&self, h: &Heap) -> String {
        match self {
            Val::Num(n) => n.to_string(),
            Val::BuiltIn(b) => b.to_string(),
            Val::Unit => String::from("{}"),
            Val::Bool(b) => String::from(if *b { "true" } else { "false" }),

            // TODO pretty printing
            Val::Fn(_) => String::from("<fn>"),
            Val::Iter(_) => String::from("<iter>"),

            Val::Str(addr) => h.to_string(*addr),
            Val::Arr(addr) => h.to_string(*addr),
            Val::Char(c) => (*c as char).to_string(),
            Val::Sym(addr) => h.to_string(*addr),
        }
    }
    pub unsafe fn unwrap<T>(&self) -> T {
        use std::mem::transmute_copy;
        match self {
            Val::Num(Num::Int(i)) => transmute_copy::<i64, T>(i),
            Val::Num(Num::Float(f)) => transmute_copy::<f64, T>(f),
            Val::Bool(b) => transmute_copy::<bool, T>(b),
            Val::Char(c) => transmute_copy::<u8, T>(c),
            Val::Arr(arr) => transmute_copy::<u64, T>(arr),
            Val::Str(s) => transmute_copy::<u64, T>(s),
            _ => unreachable!()
        }
    }
    pub fn get_type(&self, h: &Heap) -> Type {
        match self {
            Val::Num(Num::Int(_)) => Type::Int,
            Val::Num(Num::Float(_)) => Type::Float,
            Val::BuiltIn(_) => Type::BuiltIn,
            Val::Unit => Type::Unit,
            Val::Bool(_) => Type::Bool,
            Val::Fn(id) => {
                h.type_fn(*id)
            }
            Val::Iter(_) => Type::Iter,
            Val::Str(_) => Type::Str,
            Val::Arr(_) => Type::Arr,
            Val::Char(_) => Type::Char,
            Val::Sym(_) => Type::Sym(SymT::Any),
        }
    }
    pub fn type_as_string(&self, h: &Heap) -> String {
        self.get_type(h).to_string()
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
                            let msg = format!("Attempt to set string element to non-Char value of type {}.", other.type_as_string(h));
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
                let msg = format!("Attempt to index into a {}.", other.type_as_string(h));
                Err(Error { special: None,
                    msg, line: index.lb.line, col_end: index.rb.col_end, col_start: index.lb.col_start
                })
            }
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum SymT {
    Any,
    Z,
    Symbol,
}

impl SymT {
    fn type_error_msg(recieved: &SymExpr, expected: &SymT) -> String {
        format!("Cannot convert {} to {}.", recieved.kind_name(), expected.to_string())
    }
    pub fn coerce(&self, sym: SymExpr) -> Result<SymExpr, String> {
        use SymT::*;

        macro_rules! type_err {
            () => {
                Err(Self::type_error_msg(&sym, self))
            };
        }

        match self {
            Any => Ok(sym),
            Symbol => match sym {
                SymExpr::Symbol(_) => Ok(sym),
                _ => type_err!(),
            },
            Z => match sym {
                SymExpr::Z(_) => Ok(sym),
                _ => type_err!(),
            },
        }
    }
    fn to_string(&self) -> String {
        match self {
            SymT::Z => String::from("Z"),
            SymT::Any => String::from("Sym"),
            SymT::Symbol => String::from("Symbol"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Any,
    Int,
    Float,
    BuiltIn,
    Bool,
    Unit,
    Str,
    Arr,
    Char,
    Auto,
    Iter,
    Fn(Vec<Type>, Box<Type>),
    Sym(SymT),
}

impl Type {
    fn to_string(&self) -> String {
        use Type::*;
        match self {
            Any => String::from("Any"),
            Int => String::from("Int"),
            Float => String::from("Float"),
            BuiltIn => String::from("BuiltIn"),
            Bool => String::from("Bool"),
            Unit => String::from("Unit"),
            Str => String::from("Str"),
            Arr => String::from("Arr"),
            Char => String::from("Char"),
            Auto => String::from("Auto"),
            Sym(SymT::Z) => String::from("Z"),
            Sym(SymT::Any) => String::from("Sym"),
            Sym(SymT::Symbol) => String::from("Symbol"),
            Fn(param, ret) =>
                format!("({} -> {})", param.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "), ret.to_string()),
            Iter => String::from("Iter"),
        }
    }
    fn gen_type_error(expected: &Type, found: &Type) -> String {
        format!("Expected type {} but found {}.", expected.to_string(), found.to_string())
    }
    pub fn coerce(&self, v: Val, h: &Heap) -> Result<Val, String> {
        match self {
            Self::Any | Self::Auto => Ok(v),
            Self::Float => match v {
                Val::Num(Num::Float(_)) => Ok(v),
                Val::Num(Num::Int(n)) => Ok(Val::Num(Num::Float(n as f64))),
                other => Err(Self::gen_type_error(&Type::Float, &other.get_type(h)))
            },
            Self::Int => match v {
                Val::Num(Num::Float(f)) => Ok(Val::Num(Num::Int(f as i64))),
                Val::Num(Num::Int(_)) => Ok(v),
                other => Err(Self::gen_type_error(&Type::Int, &other.get_type(h))),
            },
            Self::Sym(SymT::Z) => match v{
                Val::Num(Num::Int(n)) => Ok(Val::Sym(h.alloc(HeapVal::Sym(SymExpr::Z(n.to_bigint().unwrap()))))),
                other => Err(Self::gen_type_error(&Type::Sym(SymT::Z), &other.get_type(h)))
            }
            _ => {
                let other = v.get_type(h);
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
pub struct FnVal {
    pub params: Vec<(Tok, Type)>,
    pub return_t: Type,
    pub body: Block,
}

impl FnVal {
    pub fn from(p: Fn) -> Self {
        Self { params: p.params, return_t: p.return_t, body: p.body }
    }
    pub fn call(&self, c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() != self.params.len() {
            let msg = format!("Function expected {} arguments but received {}.", self.params.len(), c.args.len());
            return Err(Error {
                special: None, msg, col_start: c.identifier.col_start, col_end: c.rparen.col_end, line: c.identifier.line
            });
        }

        let scope = Interpreter::from(&i);
        for (k, arg) in c.args.iter().enumerate() {
            let val = i.expr(&arg)?;
            let (identifier, t) = &self.params[k];
            let val = t.coerce(val, &i.heap).map_err(|msg| {
                let msg = format!("{} (at argument {})", msg, k + 1);
                Error{ special: None,
                    msg, col_start: c.identifier.col_start, col_end: c.rparen.col_end, line: c.identifier.line 
                }
            })?;
            scope.env.put(identifier.lexeme.clone(), val, &i.heap);
        }

        match scope.block(&self.body) {
            Ok(_) => {
                if self.return_t == Type::Unit || self.return_t == Type::Any {
                    Ok(Val::Unit)
                } else {
                    let msg = format!(
                        "Expected function to return value of type {}, but there was no return.",
                        self.return_t.to_string()
                    );
                    Err(Error::from(msg, &c.identifier, &c.rparen))
                }
            }
            Err(Error { special: Some(Special::Return(e, r)), .. }) => {
                if let Some(e) = e {
                    let return_val = self.return_t.coerce(scope.expr(&e)?, &i.heap).map_err(|msg| {
                        Error::from(msg, &r, &r)
                    })?;
                    Ok(return_val)
                } else {
                    Ok(Val::Unit)
                }
            }
            Err(e) => Err(e),
        }
    }
    pub fn param_count(&self) -> usize {
        self.params.len()
    }
}

