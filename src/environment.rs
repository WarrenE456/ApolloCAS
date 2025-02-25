use std::collections::HashMap;
use std::cell::RefCell;

use crate::interpreter::Val;
use crate::error::Error;
use crate::scanner::Tok;

pub struct Env<'a> {
    parent: Option<&'a Env<'a>>,
    mp: RefCell<HashMap<String, Val>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Self {
        Self {parent: None, mp: HashMap::new().into() }
    }
    pub fn def(&self, identifier: String, val: Val) {
        self.mp.borrow_mut().insert(identifier, val);
    }
    pub fn get(&self, tok: &Tok) -> Result<Val, Error> {
        let identifier = tok.lexeme.to_owned();
        self.mp.borrow().get(&identifier).ok_or(Error{
            msg: format!("Use of undefined variable '{}'.", identifier),
            line: tok.line, col_end: tok.col_end, col_start: tok.col_start,
        }).map(|v| v.clone())
    }
}
