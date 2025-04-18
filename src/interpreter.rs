use std::fmt;

use crate::error::{Error, Special};
use crate::statement::*;
use crate::scanner::{Tok, TokType};
use crate::environment::Env;

use crate::graph::GraphSignal;

use std::sync::mpsc::Sender;

#[derive(Clone)]
pub enum Val {
    Number(f64),
    Function(Vec<String>, Expr),
    BuiltIn(BuiltIn),
    Bool(bool),
    Unit,
    Proc(ProcVal),
    Str(Vec<u8>),
    Arr(Vec<Val>),
}

impl Val {
    pub fn as_string(&self) -> String {
        use Val::*;
        match self {
            Number(n) => format!("{}", n),
            Function(_, e) => e.to_string(),
            BuiltIn(b) => b.to_string(),
            Unit => String::from("()"),
            Bool(b) => String::from(if *b { "true" } else { "false" }),
            // TODO print types
            Proc(_) => String::from("<procedure>"),
            Str(s) => String::from_utf8(s.clone()).unwrap(),
            Arr(a) => {
                let s = a.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", s)
            },
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
            // TODO print types
            Proc(_) => "Procedure",
            Str(_) => "String",
            Arr(_) => "Array",
        })
    }
    fn gen_out_of_range_error(index: &Index, idx: usize, len: usize) -> Error {
        let msg = format!("Index out of bounds. Indexed location {} with object of length {}.", idx, len);
        Error { special: None,
            msg, line: index.lb.line, col_start: index.lb.col_start, col_end: index.rb.col_end
        }
    }
    pub fn index(&self, idx: usize, index: &Index) -> Result<Val, Error> {
        use Val::*;
        match self {
            Arr(a) => {
                if idx >= a.len() {
                    return Err(Self::gen_out_of_range_error(index, idx, a.len()));
                }
                return Ok(a[idx].clone());
            }
            Str(s) => {
                if idx >= s.len() {
                    return Err(Self::gen_out_of_range_error(index, idx, s.len()));
                }
                return Ok(Val::Str(vec![s[idx]]));
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

#[derive(Clone)]
pub struct BuiltIn {
    t: BuiltInT
}

impl BuiltIn {
    fn is_builtin(s: &str) -> Option<Self> {
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
    fn gen_error(msg: String, c: Call) -> Error {
        Error { special: None,
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
    fn print(c: Call, i: &Interpreter) -> Result<Val, Error> {
        for arg in c.args.iter() {
            let v = i.expr(arg.clone())?;
            print!("{}", v.to_string());
        }
        Ok(Val::Unit)
    }
    fn basic(&self, c: Call, arg_count: usize, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() == arg_count {
            let a = i.expr(c.args[0].clone())?;
            match a {
                Val::Number(a) => {
                    use BuiltInT::*;
                    Ok(Val::Number(match self.t {
                        Ln => a.ln(),
                        Sqrt => a.sqrt(),
                        Sin => a.sin(),
                        Cos => a.cos(),
                        Tan => a.tan(),
                        _ => unreachable!(),
                    }))
                },
                _ => {
                    Err(Self::gen_error(
                        format!("Can't take the {} of a {}.", self.t.to_string(), a.type_as_string()), c
                    ))
                },
            }
        }
        else {
            Err(Self::gen_error(format!("The {} function takes one argument.", self.t.to_string().to_uppercase()), c))
        }

    }
    fn exit(c: Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() == 0 {
            Err(Error {
                special: Some(Special::Exit(0)), col_start: 0, col_end: 0, line: 0, msg: "".into()
            })
        }
        else if c.args.len() == 1 {
            // TODO remove this cloning
            match i.expr(c.args[0].clone())? {
                Val::Number(n) => Err(Error {
                    special: Some(Special::Exit(n as i32)), col_start: 0, col_end: 0, line: 0, msg: "".into()
                }),
                other => {
                    let msg = format!("Attempt to call the exit function with a {}. Expected a Number.", other.type_as_string());
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
    fn create(c: Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() == 1 {
            // Remove this cloning
            match i.expr(c.args[0].clone())? {
                Val::Str(s) => {
                    let name = String::from_utf8(s).unwrap();
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
    fn graph(c: Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() == 3 {
            let graph_name = i.expr(c.args[0].clone())?;
            let fn_name = i.expr(c.args[1].clone())?;
            let e = c.args[2].clone();
            match (graph_name, fn_name) {
                (Val::Str(graph_name), Val::Str(fn_name)) => {
                    let graph_name = String::from_utf8(graph_name).unwrap();
                    let fn_name = String::from_utf8(fn_name).unwrap();
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
    fn call(&self, c: Call, i: &Interpreter) -> Result<Val, Error> {
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

#[derive(Clone)]
pub struct ProcVal {
    params: Vec<String>,
    body: Block,
}

impl ProcVal {
    pub fn from(p: Proc) -> Self {
        Self { params: p.params, body: p.body }
    }
    pub fn call(&self, c: Call, i: &Interpreter) -> Result<Val, Error> {
        if c.args.len() != self.params.len() {
            let msg = format!("Procedure expected {} arguments but received {}.", self.params.len(), c.args.len());
            return Err(Error {
                special: None, msg, col_start: c.identifier.col_start, col_end: c.rparen.col_end, line: c.identifier.line
            });
        }
        let scope = Interpreter::from(&i);
        for (k, arg) in c.args.into_iter().enumerate() {
            let val = i.expr(arg)?;
            scope.env.put(self.params[k].clone(), val);
        }

        // todo remove cloning non-sense
        match scope.block(self.body.clone()) {
            Ok(_) => Ok(Val::Unit),
            Err(Error { special: Some(Special::Return(e)), .. }) => {
                if let Some(e) = e {
                    Ok(scope.expr(e)?)
                } else {
                    Ok(Val::Unit)
                }
            }
            Err(e) => Err(e),
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
    graph_tx: Sender<GraphSignal>
}

impl<'a> Interpreter<'a> {
    pub fn new(graph_tx: Sender<GraphSignal>) -> Self {
        Self { env: Env::new(), graph_tx }
    }
    pub fn from(other: &'a Interpreter) -> Self {
        Self { env: Env::from(&other.env), graph_tx: other.graph_tx.clone() }
    }
    fn literal(&self, tok: Tok) -> Result<Val, Error> {
        use TokType::*;
        match tok.t {
            Number => Ok(Val::Number(tok.lexeme.parse().unwrap())),
            True => Ok(Val::Bool(true)),
            False => Ok(Val::Bool(false)),
            Str => Ok(Val::Str(Vec::from(tok.lexeme.as_bytes()))),
            Identifier => self.env.get(&tok),
            _ => unreachable!(),
        }
    }
    fn expect_number(&self, e: Expr, line: usize, col_start: usize, col_end: usize) -> Result<f64, Error> {
        match self.expr(e)? {
            Val::Number(n) => Ok(n),
            other => {
                let msg = format!("Expected Number but found {}.", other.type_as_string());
                Err(Error { special: None, msg, line, col_start, col_end })
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
                            Error { special: None, msg, line: op.line, col_end: op.col_end, col_start: op.col_start }
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
            a => Err(Error { special: None,
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
            Val::Proc(p) => {
                p.call(c, self)
            }
            Val::Number(_) => {
                let col_start = c.identifier.col_start;
                let col_end = c.identifier.col_end;
                let line = c.identifier.line;
                return Err(
                    Error { special: None, msg: format!("Attempt to use function calling notation on a Number."), col_start, col_end, line }
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
                        Error { special: None, msg, col_start, col_end, line }
                    );
                }
                let scope = Interpreter::from(self);
                for (i, arg) in c.args.into_iter().enumerate() {
                    scope.env.put(params[i].clone(), self.expr(arg)?);
                }
                scope.expr(body)
            }
            Val::Bool(_) => {
                let col_start = c.identifier.col_start;
                let col_end = c.identifier.col_end;
                let line = c.identifier.line;
                return Err(
                    Error { special: None, msg: format!("Attempt to use function calling notation on a Bool."), col_start, col_end, line }
                );
            }
            Val::Arr(_) => unreachable!(),
            Val::BuiltIn(_) => unreachable!(),
            Val::Unit => unreachable!(),
            Val::Str(_) => unreachable!(),
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
                Err(Error { special: None, msg, col_start: e.op.col_start, col_end: e.op.col_end, line: e.op.line })
            }
        }
    }
    fn comp(&self, c: Comp) -> Result<Val, Error> {
        let mut c = c;
        let vals = vec![self.expr(c.operands.pop().unwrap())?];
        while c.operands.len() > 0 {
            let val = self.expr(c.operands.pop().unwrap())?;
            let op = c.operators.pop().unwrap();
            use TokType::*;
            let val = match (vals.last().unwrap(), &val) {
                (Val::Number(b), Val::Number(a)) => match op.t {
                    Greater => a > b,
                    GreaterEqual => a >= b,
                    Lesser => a < b,
                    LesserEqual => a <= b,
                    Equal => a == b,
                    BangEqual => a != b,
                    _ => unreachable!(),
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
    fn or(&self, o: Or) -> Result<Val, Error> {
        match (self.expr(*o.left)?, self.expr(*o.right)?) {
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
    fn and(&self, a: And) -> Result<Val, Error> {
        match (self.expr(*a.left)?, self.expr(*a.right)?) {
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
    fn arr(&self, elements: Vec<Expr>) -> Result<Val, Error> {
        let mut vals = Vec::new();
        for element in elements {
            vals.push(self.expr(element)?);
        }
        Ok(Val::Arr(vals))
    }
    fn index(&self, i: Index) -> Result<Val, Error> {
        // TODO remove cloning
        let idx = match self.expr((*i.index).clone())? {
            Val::Number(n) => n as usize,
            other => {
                let msg = format!("Attempted to use value of type {} as an index.", other.type_as_string());
                return Err(Error{ special: None,
                    msg, line: i.rb.line, col_start: i.lb.col_start, col_end: i.rb.col_end
                });
            },
        };
        // TODO remove cloning
        self.expr(*(i.expr).clone())?.index(idx, &i)
    }
    fn expr(&self, e: Expr) -> Result<Val, Error> {
        use Expr::*;
        return match e {
            Literal(tok) => self.literal(tok),
            Group(e) => self.expr(*e),
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
        let var = Val::Number(var);
        self.env.put(var_name.to_string(), var);
        // TODO remove cloneing
        self.expr(e.clone())
    }
    fn var(&self, a: Var) -> Result<(), Error> {
        let val = self.expr(a.value)?;
        self.env.def(a.identifier, val.clone())
    }
    fn set(&self, s: Set) -> Result<(), Error> {
        let val = self.expr(s.value)?;
        self.env.set(s.identifier, val.clone())
    }
    fn def(&self, d: Def) -> Result<(), Error> {
        self.env.def(
            d.identifier,
            Val::Function(d.args, d.value),
        )?;
        Ok(())
    }
    fn block(&self, b: Block) -> Result<(), Error> {
        for statement in b.statements.into_iter() {
            self.interpret(statement)?;
        }
        Ok(())
    }
    fn eif(&self, i: If) -> Result<(), Error> {
        match self.expr(i.cond)? {
            Val::Bool(b) => {
                if b {
                    self.interpret(*i.if_branch)?;
                } else {
                    if let Some(else_branch) = i.else_branch {
                        self.interpret(*else_branch)?;
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
    fn hwile(&self, w: While) -> Result<(), Error> {
        'a: loop {
            // TODO remove this cloneing non-sense
            match self.expr(w.cond.clone())? {
                Val::Bool(b) => {
                    if b {
                        // TODO remove this cloneing non-sense
                        match self.interpret((*w.body).clone()) {
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
    pub fn interpret(&'a self, stmt: Statement) -> Result<Option<Val>, Error> {
        use Statement::*;
        return match stmt {
            Expr(e) => self.expr(e).map(|e| Some(e)),
            Var(a) => {self.var(a)?; Ok(None)}
            Set(s) => {self.set(s)?; Ok(None)}
            Def(d) => {self.def(d)?; Ok(None)}
            If(i) => {self.eif(i)?; Ok(None)}
            Block(b) => {
                let inner_scope = Self::from(self);
                inner_scope.block(b)?;
                Ok(None)
            },
            While(w) => {self.hwile(w)?; Ok(None)}
            Proc(p) => {self.proc(p)?; Ok(None)}
            Break(e) => Err(e),
            Continue(e) => Err(e),
            Return(e) => Err(e),
            Command(_) => todo!(),
        };
    }
}
