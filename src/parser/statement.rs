use crate::parser::*;
use crate::parser::expr::Index;
use crate::error::Error;
use crate::scanner::tok::Tok;

#[derive(Debug, Clone)]
pub enum Statement {
    Command(Command),
    Var(Var),
    Set(Set),
    SetIndex(SetIndex),
    Def(Def),
    Expr(Expr),
    Block(Block),
    If(If),
    While(While),
    Proc(Proc),
    Break(Error),
    Continue(Error),
    Return(Error),
    For(For),
}

#[derive(Debug, Clone)]
pub struct Command {
}
#[derive(Debug, Clone)]
pub struct Var {
    pub identifier: Tok,
    pub op: Tok,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub identifier: Tok,
    pub op: Tok,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct SetIndex {
    pub index: Index,
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
pub struct For {
    pub fro: Tok,
    pub identifier: Tok,
    pub iter: Expr,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct While {
    pub hwile: Tok,
    pub cond: Expr,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub name: Tok,
    pub params: Vec<String>,
    pub body: Block,
}
