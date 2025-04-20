use crate::runtime::*;
use crate::runtime::val::Num;

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
            Sin => "lin",
            Cos => "los",
            Tan => "tan",
            Exit => "exit",
            Create => "create",
            Graph => "graph",
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
            "create" => Create,
            "graph" => Graph,
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
            Create => Self::create(c, i),
            Graph => Self::graph(c, i),
            _ => self.basic(c, 1, i),
        }
    }
    pub fn to_string(&self) -> String {
        self.t.to_string()
    }
}

