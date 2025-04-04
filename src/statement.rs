use crate::error::Error;
use crate::scanner::Tok;

#[derive(Debug, Clone)]
pub enum Statement {
    Command(Command),
    Var(Var),
    Def(Def),
    Expr(Expr),
    Block(Block),
    If(If),
    While(While),
    Break(Error),
    Continue(Error),
}

#[derive(Debug, Clone)]
pub struct Command {
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Tok),
    Group(Box<Expr>),
    Binary(Binary),
    Negate(Negate),
    Call(Call),
    Exp(Exp),
    Comp(Comp),
    Or(Or),
    And(And),
}

impl Expr {
    pub fn to_string(&self) -> String {
        use Expr::*;
        match self {
            Literal(l) => l.lexeme.clone(),
            Group(e) => format!("({})", (*e).to_string()),
            Binary(b) => {
                b.operands
                    .iter().enumerate()
                    .filter(|(i, _)| *i != 0)
                    .fold(b.operands.first().unwrap().to_string(), |acc, (i, v)| {
                        format!("{} {} {}", acc, b.operators[i - 1].lexeme, v.to_string())
                    })
            },
            Negate(n) => format!("-{}", n.value.to_string()),
            Call(c) => {
                let args = c.args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", c.identifier.lexeme, args)
            }
            Exp(e) => format!("{}^{}", e.base.to_string(), e.power.to_string()),
            Comp(c) => {
                c.operands
                    .iter().enumerate()
                    .filter(|(i, _)| *i != 0)
                    .fold(c.operands.first().unwrap().to_string(), |acc, (i, v)| {
                        format!("{} {} {}", acc, c.operators[i - 1].lexeme, v.to_string())
                    })
            }
            And(a) => format!("{} and {}", a.right.to_string(), a.left.to_string()),
            Or(o) => format!("{} or {}", o.right.to_string(), o.left.to_string()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Exp {
    pub base: Box<Expr>,
    pub power: Box<Expr>,
    pub op: Tok,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operators: Vec<Tok>,
    pub operands: Vec<Expr>,
}

impl Binary {
    pub fn new(operators: Vec<Tok>, operands: Vec<Expr>) -> Self {
        let operators = operators.into_iter().rev().collect();
        let operands = operands.into_iter().rev().collect();
        Self { operators, operands }
    }
}

#[derive(Debug, Clone)]
pub struct Negate {
    pub minus: Tok,
    pub value: Box<Expr>
}

#[derive(Debug, Clone)]
pub struct Call {
    pub identifier: Tok,
    pub args: Vec<Expr>,
    pub rparen: Tok,
}

#[derive(Debug, Clone)]
pub struct Comp {
    pub operators: Vec<Tok>,
    pub operands: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Or {
    pub op: Tok,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct And {
    pub op: Tok,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}


#[derive(Debug, Clone)]
pub struct Var {
    pub identifier: Tok,
    pub op: Tok,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub identifier: Tok,
    pub args: Vec<String>,
    pub op: Tok,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub eif: Tok,
    pub cond: Expr,
    pub if_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub hwile: Tok,
    pub cond: Expr,
    pub body: Box<Statement>,
}
