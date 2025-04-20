use std::collections::HashMap;
use std::sync::{Mutex, Weak, Arc};

use crate::runtime::val::Val;
use crate::error::Error;
use crate::scanner::tok::Tok;

pub struct Env {
    parent: Option<Arc<Env>>,
    pub children: Mutex<Vec<Weak<Env>>>,
    pub mp: Mutex<HashMap<String, Val>>,
}

impl<'a> Env {
    pub fn new() -> Self {
        Self {parent: None, mp: HashMap::new().into(), children: Mutex::new(Vec::new()) }
    }
    pub fn clear_moved_children(&self) {
        let mut children = self.children.lock().unwrap();
        children.retain(|child| child.upgrade().is_some())
    }
    pub fn from(parent: Arc<Env>) -> Arc<Self> {
        let mut parent_children = parent.children.lock().unwrap();
        let _self = Arc::new(Self {
            parent: Some(Arc::clone(&parent)), mp: HashMap::new().into(),
            children: Mutex::new(Vec::new())
        });
        parent_children.push(Arc::downgrade(&_self));
        _self
    }
    pub fn def(&self, i: Tok, val: Val) -> Result<(), Error> {
        if self.mp.lock().unwrap().contains_key(&i.lexeme) {
            let msg = format!("Attempt to redefine '{}'.", i.lexeme);
            Err(Error {
                special: None, msg, col_start: i.col_start, col_end: i.col_end, line: i.line
            })
        } else {
            self.mp.lock().unwrap().insert(i.lexeme, val);
            Ok(())
        }
    }
    pub fn put(&self, i: String, val: Val) {
        self.mp.lock().unwrap().insert(i, val);
    }
    pub fn set(&self, i: Tok, val: Val) -> Result<(), Error> {
        if !self.mp.lock().unwrap().contains_key(&i.lexeme) {
            if let Some(p) = &self.parent {
                p.set(i, val)
            } else {
                let msg = format!("Attempt to set undefined variable '{}'.", i.lexeme);
                Err(Error {
                    special: None, msg, col_start: i.col_start, col_end: i.col_end, line: i.line
                })
            }
        } else {
            self.mp.lock().unwrap().insert(i.lexeme, val);
            Ok(())
        }
    }
    pub fn get(&self, tok: &Tok) -> Result<Val, Error> {
        match self.mp.lock().unwrap().get(&tok.lexeme) {
            None => {
                if let Some(env) = &self.parent {
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
