use crate::runtime::*;
use crate::runtime::val::Num;
use std::time::{SystemTime, UNIX_EPOCH};

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
    Create,
    Graph,
    Clock,
    Sleep,
    Push,
    Pop,
    Range,
    Param,
    Len,
}

// struct Template {
//     name: String,
//     types: Vec<Type>,
// }
//
// impl Template {
//     fn call(&self, c: Call, i: &Interpreter) -> Result<Val, Error> {
//         if self.types.len() != c.args.len() {
//             let msg = format!("{} expects {} arguments but found {}.", self.name, self.types.len(), c.args.len());
//             return Err(Error { special: None,
//                 msg, col_start: c.identifier.col_start, col_end: c.rparen.col_end, line: c.identifier.line
//             })
//         }
//
//         let mut vals = Vec::new();
//         for (k, (arg, t)) in c.args.iter().zip(self.types.iter()).enumerate() {
//             let val = t.coerce(i.expr(&arg)?).map_err(|msg| {
//                 let msg = format!("{} (at argument {})", msg, k);
//                 Error { special: None,
//                     msg, col_start: c.identifier.col_start, col_end: c.rparen.col_end, line: c.identifier.line
//                 }
//             })?;
//             vals.push(val);
//         }
//
//
//     }
// }

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
            Create => "create",
            Graph => "graph",
            Clock => "clock",
            Sleep => "sleep",
            Push => "push",
            Pop => "pop",
            Range => "range",
            Param => "param",
            Len => "len",
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
            "param" => Param,
            "log" => Log,
            "ln" => Ln,
            "print" => Print,
            "println" => Println,
            "sqrt" => Sqrt,
            "sin" => Sin,
            "cos" => Cos,
            "tan" => Tan,
            "exit" => Exit,
            "create" => Create,
            "graph" => Graph,
            "clock" => Clock,
            "sleep" => Sleep,
            "push" => Push,
            "pop" => Pop,
            "range" => Range,
            "len" => Len,
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
                        a.type_as_string(), b.type_as_string(), 
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
                    Err(Self::gen_error(format!("Can't take the log of a {}.", a.type_as_string()), c))
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
                        format!("Can't take the {} of a {}.", self.t.to_string(), a.type_as_string()), &c
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
                    let msg = format!("Attempt to call the exit function with a {}. Expected an Int.", other.type_as_string());
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
    fn create(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() == 1 {
            match i.expr(&c.args[0])? {
                Val::Str(addr) => {
                    let name = i.heap.to_string(addr);
                    i.graph_tx.send(GraphSignal::Create(name)).unwrap();
                    Ok(Val::Unit)
                }
                other => {
                    let msg = format!(
                        "'create' expects a string (the name of the graph to be created) but found a {}.",
                        other.type_as_string()
                    );
                    Err(Error {
                        special: None, msg, col_start: c.identifier.col_start, col_end: c.rparen.col_end, line: c.identifier.line
                    })
                },
            }
        } else {
            let msg = format!("'create' expects one argument (the graph name), but found {}.", c.args.len());
            Err(Error {
                special: None, msg, col_start: c.identifier.col_start, col_end: c.rparen.col_end, line: c.identifier.line
            })
        }
    }
    fn graph(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() == 3 {
            let graph_name = i.expr(&c.args[0])?;
            let fn_name = i.expr(&c.args[1])?;
            let e = c.args[2].clone();
            match (graph_name, fn_name) {
                (Val::Str(graph_addr), Val::Str(fn_addr)) => {
                    let graph_name = i.heap.to_string(graph_addr);
                    let fn_name = i.heap.to_string(fn_addr);
                    i.graph_tx.send(GraphSignal::Graph{ graph_name, fn_name, e}).unwrap();
                    Ok(Val::Unit)
                }
                (other1, other2) => {
                    let msg = format!(
                        "'graph' expects a 2 strings (the name of the graph and function) and an expression but found a {} and {}.",
                        other1.type_as_string(), other2.type_as_string(),
                    );
                    Err(Error {
                        special: None, msg, col_start: c.identifier.col_start, col_end: c.rparen.col_end, line: c.identifier.line
                    })
                },
            }
        } else {
            let msg = format!("'graph' expects three argument (graph name, function name, expression), but found {}.", c.args.len());
            Err(Error {
                special: None, msg, col_start: c.identifier.col_start, col_end: c.rparen.col_end, line: c.identifier.line
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
            let val = t.coerce(i.expr(arg)?).map_err(|msg| {
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
                        let msg = format!("Attempt to push a non-Char value of type {} to a Str.", other.type_as_string());
                        return Err(Error::from(msg, &c.identifier, &c.rparen));
                    }
                }
            }
            other => {
                let msg = format!("Attempt to push into a value of type {}.", other.type_as_string());
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
                let msg = format!("Attempt to pop from value of type {}.", other.type_as_string());
                Err(Error::from(msg, &c.identifier, &c.rparen))
            }
        }
    }
    fn range(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() == 1 {
            let index = unsafe {
                Type::Int
                    .coerce(i.expr(&c.args[0])?)
                    .map_err(|msg| Error::from(msg,&c.identifier,&c.rparen))?
                    .unwrap::<i64>()
            };
            if index < 0 {
                let msg = String::from("Inputs to range function must be positive.");
                return Err(Error::from(msg, &c.identifier, &c.rparen));
            }
            let addr = i.heap.alloc(HeapVal::Arr((0..index).map(|i| Val::Num(Num::Int(i))).collect::<Vec<_>>()));
            Ok(Val::Arr(addr))
        } else if c.args.len() == 3 {
            todo!()
        } else {
            let msg= format!("Range expected 1 or 3 arguments, but found {}.", c.args.len());
            return Err(Error::from(msg, &c.identifier, &c.rparen));
        }
    }
    fn param(c: &Call, i: &Interpreter) -> Result<Val, Error> {
        use Type::*;
        let args = Self::type_check(vec![Str, Float, Float, Float, Float, Bool], c, i)?;
        let graph_name_id = unsafe { args[0].unwrap::<u64>() };
        let graph_name = match i.heap.get(graph_name_id) {
            Some(HeapVal::Str(s)) => String::from_utf8(s).unwrap(),
            _ => unreachable!(),
        };
        let minx = unsafe { args[1].unwrap::<f64>() };
        let maxx = unsafe { args[2].unwrap::<f64>() };
        let miny = unsafe { args[3].unwrap::<f64>() };
        let maxy = unsafe { args[4].unwrap::<f64>() };
        let grid_lines = unsafe { args[5].unwrap::<bool>() };
        let _ = i.graph_tx.send(GraphSignal::Set {
            graph_name, minx, maxx, miny, maxy, grid_lines
        });
        Ok(Val::Unit)
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
                other => {
                    let msg = format!(
                        "'len' expects an argument of type Str or Arr, but found value of type {}.",
                        other.type_as_string()
                    );
                    Err(Error::from(msg, &c.identifier, &c.rparen))
                }
            }
        }
    }
    pub fn call(&self, c: &Call, i: &Interpreter) -> Result<Val, Error> {
        use BuiltInT::*;
        match self.t {
            Param => Self::param(c, i),
            Log => Self::log(c, i),
            Print => Self::print(c, i),
            Println => {
                Self::print(c, i)?;
                println!("");
                Ok(Val::Unit)
            }
            Exit => Self::exit(c, i),
            Create => Self::create(c, i),
            Graph => Self::graph(c, i),
            Clock => Self::clock(c, i),
            Sleep => Self::sleep(c, i),
            Push => Self::push(c, i),
            Pop => Self::pop(c, i),
            Range => Self::range(c, i),
            Len => Self::len(c, i),
            _ => self.basic(c, 1, i),
        }
    }
    pub fn to_string(&self) -> String {
        self.t.to_string()
    }
}


