use std::time::{SystemTime, UNIX_EPOCH};

use crate::runtime::*;
use crate::runtime::val::Num;
use crate::mem::heap::{Iter, Range};

// TODO inverse trig and other other asortment of other asortments of other assortermentnetms... of functions
// round floor ceil
#[derive(Clone, Copy, Debug)]
enum BuiltInT {
    Log,
    Print,
    Println,
    Ln,
    Sqrt,
    Sin,
    Cos,
    Tan,
    Exit,
    Clock,
    Sleep,
    Push,
    Pop,
    Range,
    Len,
    Copy,
    Type,
    Iter,
    Next,
    Gcd,
}

impl BuiltInT {
    pub fn to_string(&self) -> String {
        use BuiltInT::*;
        String::from(match self {
            Log => "log",
            Ln => "ln",
            Print => "print",
            Println => "println",
            Sqrt => "sqrt",
            Sin => "sin",
            Cos => "cos",
            Tan => "tan",
            Exit => "exit",
            Clock => "clock",
            Sleep => "sleep",
            Push => "push",
            Pop => "pop",
            Range => "range",
            Len => "len",
            Copy => "copy",
            Type => "type",
            Iter => "iter",
            Next => "next",
            Gcd => "gcd",
        })
    }
}

#[derive(Clone, Debug)]
pub struct BuiltIn {
    t: BuiltInT
}

impl BuiltIn {
    pub fn is_builtin(s: &str) -> Option<Self> {
        use BuiltInT::*;
        let t = match s {
            "log" => Log,
            "ln" => Ln,
            "print" => Print,
            "println" => Println,
            "sqrt" => Sqrt,
            "sin" => Sin,
            "cos" => Cos,
            "tan" => Tan,
            "exit" => Exit,
            "clock" => Clock,
            "sleep" => Sleep,
            "push" => Push,
            "pop" => Pop,
            "range" => Range,
            "len" => Len,
            "copy" => Copy,
            "type" => Type,
            "iter" => Iter,
            "next" => Next,
            "gcd" => Gcd,
            _ => return None,
        };
        Some(BuiltIn { t })
    }
    fn gen_error(msg: String, c: &Call) -> Error {
        Error { special: None,
            msg,
            line: c.identifier.line,
            col_start: c.identifier.col_start,
            col_end: c.rparen.col_end,
        }
    }
    fn log(c:& Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() == 2 {
            let base = i.expr(&c.args[0].clone())?;
            let arg2 = i.expr(&c.args[1].clone())?;
            match (base, arg2) {
                (Val::Num(base), Val::Num(x)) => {
                    Ok(Val::Num(Num::Float(x.to_float().log(base.to_float()))))
                }
                (a, b) => {
                    let msg = format!(
                        "Attempt take the log of a {} with the base of a {}. Both should be numbers.",
                        a.type_as_string(&i.heap), b.type_as_string(&i.heap), 
                    );
                    Err(Self::gen_error(msg, c))
                }
            }
        }
        else if c.args.len() == 1 {
            let a = i.expr(&c.args[0])?;
            match a {
                Val::Num(a) => Ok(Val::Num(Num::Float(a.to_float().log10()))),
                _ => {
                    Err(Self::gen_error(format!("Can't take the log of a {}.", a.type_as_string(&i.heap)), c))
                },
            }
        }
        else {
            Err(Self::gen_error(String::from("Log takes one or two arguments."), c))
        }
    }
    fn print(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        for arg in c.args.iter() {
            let v = i.expr(&arg)?;
            print!("{}", v.to_string(&i.heap));
        }
        Ok(Val::Unit)
    }
    fn basic(&self, c: &Call, arg_count: usize, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() == arg_count {
            let a = i.expr(&c.args[0])?;
            match a {
                Val::Num(a) => {
                    use BuiltInT::*;
                    let a = a.to_float();
                    Ok(Val::Num(Num::Float(match self.t {
                        Ln => a.ln(),
                        Sqrt => a.sqrt(),
                        Sin => a.sin(),
                        Cos => a.cos(),
                        Tan => a.tan(),
                        _ => unreachable!(),
                    })))
                },
                _ => {
                    Err(Self::gen_error(
                        format!("Can't take the {} of a {}.", self.t.to_string(), a.type_as_string(&i.heap)), &c
                    ))
                },
            }
        }
        else {
            Err(Self::gen_error(format!("The {} function takes one argument.", self.t.to_string().to_uppercase()), &c))
        }

    }
    fn exit(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() == 0 {
            Err(Error {
                special: Some(Special::Exit(0)), col_start: 0, col_end: 0, line: 0, msg: "".into()
            })
        }
        else if c.args.len() == 1 {
            match i.expr(&c.args[0])? {
                Val::Num(Num::Int(n)) => Err(Error {
                    special: Some(Special::Exit(n as i32)), col_start: 0, col_end: 0, line: 0, msg: "".into()
                }),
                other => {
                    let msg = format!("Attempt to call the exit function with a {}. Expected an Int.", other.type_as_string(&i.heap));
                    Err(Error {
                        special: None, msg, col_start: c.identifier.col_start, col_end: c.rparen.col_end, line: c.identifier.line
                    })
                }
            }
        } else {
            let msg = format!("The exit function takes 0 or 1 arguments, but {} were provided.", c.args.len());
            Err(Error {
                special: None, msg, col_start: c.identifier.col_start, col_end: c.identifier.col_end, line: c.identifier.line
            })
        }
    }
    fn type_check(t: Vec<Type>, c: &Call, i: &Interpreter) -> Result<Vec<Val>, Error> {
        if t.len() != c.args.len() {
            let msg = format!("Expected {} arguments but found {}.", t.len(), c.args.len());
            return Err(Error { special: None,
                msg, col_start: c.identifier.col_start, col_end: c.identifier.col_end, line: c.identifier.line
            })
        }
        let mut vals = Vec::new();
        for (k, (arg, t)) in c.args.iter().zip(t.iter()).enumerate() {
            let val = t.coerce(i.expr(arg)?, &i.heap).map_err(|msg| {
                let msg = format!("{} (at argument {})", msg, k + 1);
                Error { special: None,
                    msg, col_start: c.identifier.col_start, col_end: c.rparen.col_end, line: c.identifier.line
                }
            })?;
            vals.push(val);
        }
        Ok(vals)
    }
    fn clock(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        let _ = Self::type_check(vec![], c, i)?;
        let current_system_time = SystemTime::now();
        let duration_since_epoch = current_system_time.duration_since(UNIX_EPOCH).unwrap();
        let milliseconds_timestamp  = duration_since_epoch.as_millis() as i64;
        Ok(Val::Num(Num::Int(milliseconds_timestamp)))
    }
    fn sleep(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        let vals = Self::type_check(vec![Type::Int], c, i)?;
        let ms = unsafe { vals[0].unwrap::<i64>() } as u64;
        std::thread::sleep(std::time::Duration::from_millis(ms));
        Ok(Val::Unit)
    }
    fn push(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() != 2 {
            let msg= format!("push expected 2 arguments (the Arr/Str, and the value to push), but found {}.", c.args.len());
            return Err(Error::from(msg, &c.identifier, &c.rparen));
        }
        match i.expr(&c.args[0])? {
            Val::Arr(addr) => {
                i.heap.push_arr(addr, i.expr(&c.args[1])?);
            }
            Val::Str(addr) => {
                match i.expr(&c.args[1])? {
                    Val::Char(c) => i.heap.push_str(addr, c),
                    other => {
                        let msg = format!("Attempt to push a non-Char value of type {} to a Str.", other.type_as_string(&i.heap));
                        return Err(Error::from(msg, &c.identifier, &c.rparen));
                    }
                }
            }
            other => {
                let msg = format!("Attempt to push into a value of type {}.", other.type_as_string(&i.heap));
                return Err(Error::from(msg, &c.identifier, &c.rparen));
            }
        }
        Ok(Val::Unit)
    }
    fn pop(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() != 1 {
            let msg= format!("pop expected 1 argument (the Arr/Str to pop from), but found {}.", c.args.len());
            return Err(Error::from(msg, &c.identifier, &c.rparen));
        }
        match i.expr(&c.args[0])? {
            Val::Arr(addr) => {
                Ok(i.heap.pop_arr(addr).unwrap_or(Val::Unit))
            }
            Val::Str(addr) => {
                Ok(i.heap.pop_arr(addr).unwrap_or(Val::Unit))
            }
            other => {
                let msg = format!("Attempt to pop from value of type {}.", other.type_as_string(&i.heap));
                Err(Error::from(msg, &c.identifier, &c.rparen))
            }
        }
    }
    fn range(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() == 1 {
            let index = unsafe {
                Type::Int
                    .coerce(i.expr(&c.args[0])?, &i.heap)
                    .map_err(|msg| Error::from(msg,&c.identifier,&c.rparen))?
                    .unwrap::<i64>()
            };
            let range = HeapVal::Iter(Iter::Range(Range::new(0, index, 1)));
            let addr = i.heap.alloc(range);
            Ok(Val::Iter(addr))
        } else if c.args.len() == 3 {
            let start = unsafe {
                Type::Int
                    .coerce(i.expr(&c.args[0])?, &i.heap)
                    .map_err(|msg| Error::from(msg,&c.identifier,&c.rparen))?
                    .unwrap::<i64>()
            };
            let stop = unsafe {
                Type::Int
                    .coerce(i.expr(&c.args[1])?, &i.heap)
                    .map_err(|msg| Error::from(msg,&c.identifier,&c.rparen))?
                    .unwrap::<i64>()
            };
            let inc = unsafe {
                Type::Int
                    .coerce(i.expr(&c.args[2])?, &i.heap)
                    .map_err(|msg| Error::from(msg,&c.identifier,&c.rparen))?
                    .unwrap::<i64>()
            };
            let range = HeapVal::Iter(Iter::Range(Range::new(start, stop, inc)));
            let addr = i.heap.alloc(range);
            Ok(Val::Iter(addr))
        } else {
            let msg= format!("Range expected 1 or 3 arguments, but found {}.", c.args.len());
            return Err(Error::from(msg, &c.identifier, &c.rparen));
        }
    }
    fn len(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() != 1 {
            let msg = format!(
                "'len' expects 1 argument, the string or array which you wish to know the name of, but found {}.",
                c.args.len()
            );
            Err(Error::from(msg, &c.identifier, &c.rparen))
        } else {
            match i.expr(&c.args[0])? {
                Val::Str(s) => Ok(Val::Num(Num::Int(i.heap.len(s) as i64))),
                Val::Arr(a) => Ok(Val::Num(Num::Int(i.heap.len(a) as i64))),
                Val::Fn(f) => Ok(Val::Num(Num::Int(i.heap.len(f) as i64))),
                other => {
                    let msg = format!(
                        "'len' expects an argument of type Str or Arr, but found value of type {}.",
                        other.type_as_string(&i.heap)
                    );
                    Err(Error::from(msg, &c.identifier, &c.rparen))
                }
            }
        }
    }
    fn copy(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() != 1 {
            let msg = String::from("'copy' expects one argument.");
            Err(Error::from(msg, &c.identifier, &c.rparen))
        } else {
            let mut val = i.expr(&c.args[0])?;
            match &mut val {
                Val::Str(addr) 
                | Val::Arr(addr)
                | Val::Iter(addr)
                | Val::Fn(addr) 
                | Val::Sym(addr) => {
                    *addr = i.heap.alloc(i.heap.get(*addr).unwrap().clone());
                }
                Val::Unit | Val::Char(_) | Val::Num(_)
                | Val::Bool(_) | Val::BuiltIn(_)        => {}
            }
            Ok(val)
        }
    }
    fn _type(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() != 1 {
            let msg = String::from("'type' expects one argument.");
            Err(Error::from(msg, &c.identifier, &c.rparen))
        } else {
            let typename = HeapVal::Str(i.expr(&c.args[0])?.type_as_string(&i.heap).into());
            let addr = i.heap.alloc(typename);
            Ok(Val::Str(addr))
        }
    }
    fn iter(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() != 1 {
            let msg = String::from("'iter' expects one argument.");
            Err(Error::from(msg, &c.identifier, &c.rparen))
        } else {
            match i.expr(&c.args[0])? {
                Val::Arr(addr) | Val::Str(addr) => {
                    let iter = HeapVal::Iter(Iter::Heap(HeapIter::new(addr)));
                    let addr = i.heap.alloc(iter);
                    Ok(Val::Iter(addr))
                }
                Val::Iter(i) => Ok(Val::Iter(i)),
                other => {
                    let msg = format!("'iter' cannot turn type {} into an iterator.", other.type_as_string(&i.heap));
                    Err(Error::from(msg, &c.identifier, &c.rparen))
                }
            }
        }
    }
    fn next(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() != 1 {
            let msg = String::from("'next' expects one argument, an iterator.");
            Err(Error::from(msg, &c.identifier, &c.rparen))
        } else {
            match i.expr(&c.args[0])? {
                Val::Iter(iter) => Ok(i.heap.next_iter(iter).unwrap_or(Val::Unit)),
                other => {
                    let msg = format!("'next' expects an  Iter, but found {}.", other.type_as_string(&i.heap));
                    Err(Error::from(msg, &c.identifier, &c.rparen))
                }
            }
        }
    }
    fn gcd(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() != 2 {
            let msg = format!("'gcd' expects 2 arguments, but found {}", c.args.len());
            return Err(Error::from_call(msg, c));
        }

        match (i.expr(&c.args[0])?, i.expr(&c.args[1])?) {
            (Val::Num(a), Val::Num(b)) => Ok(Val::Num(Num::gcd(a, b))),
            (Val::Sym(a), Val::Sym(b)) => todo!(),
            (Val::Sym(a), b)
            | (b, Val::Sym(a)) => todo!(),
            (a, b) => {
                let msg = format!(
                    "Cannot take the gcd of values of type {} and {}.",
                    a.type_as_string(&i.heap), b.type_as_string(&i.heap)
                );
                Err(Error::from_call(msg, c))
            }
        }
    }
    pub fn call(&self, c: &Call, i: &Interpreter) -> Result<Val, Error> {
        use BuiltInT::*;
        match self.t {
            Log => Self::log(c, i),
            Print => Self::print(c, i),
            Println => {
                Self::print(c, i)?;
                println!("");
                Ok(Val::Unit)
            }
            Exit => Self::exit(c, i),
            Clock => Self::clock(c, i),
            Sleep => Self::sleep(c, i),
            Push => Self::push(c, i),
            Pop => Self::pop(c, i),
            Range => Self::range(c, i),
            Len => Self::len(c, i),
            Copy => Self::copy(c, i),
            Type => Self::_type(c, i),
            Iter => Self::iter(c, i),
            Next => Self::next(c, i),
            Gcd => Self::gcd(c, i),
            _ => self.basic(c, 1, i),
        }
    }
    pub fn to_string(&self) -> String {
        self.t.to_string()
    }
}


