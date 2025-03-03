use crate::scanner::Tok;

#[derive(Debug)]
pub enum Statement {
    Command(Command),
    Var(Var),
    Def(Def),
    Expr(Expr),
}

#[derive(Debug)]
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
                        format!("{} {} {}", acc, b.ops[i - 1].lexeme, v.to_string())
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
    pub ops: Vec<Tok>,
    pub operands: Vec<Expr>,
}

impl Binary {
    pub fn new(ops: Vec<Tok>, operands: Vec<Expr>) -> Self {
        Self { ops, operands }
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
    pub lparen: Tok,
}

#[derive(Debug)]
pub struct Var {
    pub identifier: Tok,
    pub op: Tok,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Def {
    pub identifier: Tok,
    pub args: Vec<String>,
    pub op: Tok,
    pub value: Expr,
}
