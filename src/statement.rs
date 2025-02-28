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
}

impl Expr {
    pub fn to_string(&self) -> String {
        use Expr::*;
        match self {
            Literal(l) => l.lexeme.clone(),
            Group(e) => format!("({})", (*e).to_string()),
            Binary(b) => format!("{} {} {}", b.l.to_string(), b.op.lexeme.clone(), b.r.to_string()),
            Negate(n) => format!("-{}", n.value.to_string()),
            Call(c) => {
                let args = c.args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", c.identifier.lexeme, args)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub l: Box<Expr>,
    pub op: Tok,
    pub r: Box<Expr>,
}

impl Binary {
    pub fn new(l: Expr, op: Tok, r: Expr) -> Self {
        Self { l: Box::from(l), op, r: Box::from(r) }
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
