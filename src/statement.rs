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
    Binary(Binary),
    Negate(Negate),
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
