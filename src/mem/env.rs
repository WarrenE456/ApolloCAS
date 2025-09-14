use std::collections::HashMap;
use std::sync::{Mutex, Weak, Arc};

use crate::runtime::val::{Val, Type};
use crate::mem::heap::Heap;
use crate::error::Error;
use crate::scanner::tok::Tok;

pub struct Env {
    parent: Option<Arc<Env>>,
    pub children: Mutex<Vec<Weak<Env>>>,
    pub mp: Mutex<HashMap<String, (Type, Val)>>,
}

impl<'a> Env {
    pub fn new() -> Self {
        Self { parent: None, mp: HashMap::new().into(), children: Mutex::new(Vec::new()) }
    }
    pub fn clear_moved_children(&self) {
        let mut children = self.children.lock().unwrap();
        children.retain(|child| child.upgrade().is_some())
    }
    pub fn from(parent: Arc<Env>) -> Arc<Self> {
        let _self = Arc::new(Self {
            parent: Some(Arc::clone(&parent)), mp: HashMap::new().into(),
            children: Mutex::new(Vec::new())
        });
        {
            let mut parent_children = parent.children.lock().unwrap();
            parent_children.push(Arc::downgrade(&_self));
        }
        _self
    }
    pub fn def(&self, i: Tok, val: Val, t: Type) -> Result<(), Error> {
        let contains = self.mp.lock().unwrap().contains_key(&i.lexeme);
        if  contains {
            let msg = format!("Attempt to redefine '{}'.", i.lexeme);
            Err(Error {
                special: None, msg, col_start: i.col_start, col_end: i.col_end, line: i.line
            })
        } else {
            self.mp.lock().unwrap().insert(i.lexeme, (t, val));
            Ok(())
        }
    }
    pub fn put(&self, i: String, val: Val, h: &Heap) {
        self.mp.lock().unwrap().insert(i, (val.get_type(h), val));
    }
    pub fn set(&self, i: Tok, val: Val, heap: &Heap) -> Result<(), Error> {
        let contains = self.mp.lock().unwrap().contains_key(&i.lexeme);
        if !contains {
            if let Some(p) = &self.parent {
                p.set(i, val, heap)
            } else {
                let msg = format!("Attempt to set undefined variable '{}'.", i.lexeme);
                Err(Error {
                    special: None, msg, col_start: i.col_start, col_end: i.col_end, line: i.line
                })
            }
        } else {
            let (t, _) = self.mp.lock().unwrap().get(&i.lexeme).unwrap().clone();
            let val = t.coerce(val, heap).map_err(|msg| {
                Error {
                    special: None, msg, col_start: i.col_start, col_end: i.col_end, line: i.line
                }
            })?;
            self.mp.lock().unwrap().insert(i.lexeme, (t, val));
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
            Some((_,v)) => Ok(v.clone()),
        }
    }
}
