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
    pub fn from(other: &'a Env<'a>) -> Self {
        Self {parent: Some(other), mp: HashMap::new().into() }
    }
    pub fn def(&self, i: Tok, val: Val) -> Result<(), Error> {
        if self.mp.borrow().contains_key(&i.lexeme) {
            let msg = format!("Attempt to redefine '{}'.", i.lexeme);
            Err(Error {
                special: None, msg, col_start: i.col_start, col_end: i.col_end, line: i.line
            })
        } else {
            self.mp.borrow_mut().insert(i.lexeme, val);
            Ok(())
        }
    }
    pub fn put(&self, i: String, val: Val) {
        self.mp.borrow_mut().insert(i, val);
    }
    pub fn set(&self, i: Tok, val: Val) -> Result<(), Error> {
        if !self.mp.borrow().contains_key(&i.lexeme) {
            if let Some(p) = self.parent {
                p.set(i, val)
            } else {
                let msg = format!("Attempt to set undefined variable '{}'.", i.lexeme);
                Err(Error {
                    special: None, msg, col_start: i.col_start, col_end: i.col_end, line: i.line
                })
            }
        } else {
            self.mp.borrow_mut().insert(i.lexeme, val);
            Ok(())
        }
    }
    pub fn get(&self, tok: &Tok) -> Result<Val, Error> {
        match self.mp.borrow().get(&tok.lexeme) {
            None => {
                if let Some(env) = self.parent {
                    env.get(tok)
                } else {
                    Err(Error{
                        msg: format!("Use of undefined variable '{}'.", tok.lexeme),
                        line: tok.line, col_end: tok.col_end, col_start: tok.col_start,
                        special: None,
                    })
                }
            }
            Some(v) => Ok(v.clone()),
        }
    }
}
