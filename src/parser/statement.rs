use crate::parser::*;
use crate::parser::expr::Index;
use crate::error::Error;
use crate::scanner::tok::Tok;
use crate::runtime::val::Type;

#[derive(Debug, Clone)]
pub enum Statement {
    Var(Var),
    Set(Set),
    SetIndex(SetIndex),
    Expr(Expr),
    Block(Block),
    If(If),
    While(While),
    Fn(Fn),
    Break(Error),
    Continue(Error),
    Return(Error),
    For(For),
}

#[derive(Debug, Clone)]
pub struct Var {
    pub identifier: Tok,
    pub t: Type,
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
pub struct Fn {
    pub name: Tok,
    pub params: Vec<(Tok, Type)>,
    pub return_t: Type,
    pub body: Block,
}
