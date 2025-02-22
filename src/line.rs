use crate::scanner::Tok;

#[derive(Debug)]
pub enum Line<'a> {
    Command(Command),
    Expr(Expr<'a>),
}

#[derive(Debug)]
pub struct Command {
}

#[derive(Debug)]
pub enum Expr<'a> {
    Literal(Tok<'a>),
    Binary(Binary<'a>),
}

#[derive(Debug)]
pub struct Binary<'a> {
    pub l: Box<Expr<'a>>,
    pub op: Tok<'a>,
    pub r: Box<Expr<'a>>,
}

impl<'a> Binary<'a> {
    pub fn new(l: Expr<'a>, op: Tok<'a>, r: Expr<'a>) -> Self {
        Self { l: Box::from(l), op, r: Box::from(r) }
    }
}
