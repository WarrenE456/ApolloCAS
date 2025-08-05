use std::sync::RwLock;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering::SeqCst;
use std::collections::{HashMap, HashSet};

use crate::runtime::val::Val;

#[derive(Clone, Debug)]
pub enum HeapVal {
    Str(Vec<u8>),
    Arr(Vec<Val>),
}

pub struct HeapIter {
    addr: u64,
    idx: usize
}

impl HeapIter {
    pub fn new(addr: u64) -> Self{
        Self { addr, idx: 0 }
    }
    pub fn next(&mut self, h: &Heap) -> Option<Val> {
        if self.idx < h.len(self.addr) {
            self.idx += 1;
            Some(h.get_at(self.addr, self.idx - 1)?)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Heap {
    counter: AtomicU64,
    mem: RwLock<HashMap<u64, HeapVal>>,
    marks: RwLock<HashMap<u64, bool>>,
    hidden_ref: RwLock<HashSet<u64>>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            counter: AtomicU64::new(0), mem: RwLock::new(HashMap::new()),
            marks: RwLock::new(HashMap::new()), hidden_ref: RwLock::new(HashSet::new())
        }
    }
    pub fn reset_marks(&self) {
        self.marks.write().unwrap().iter_mut().for_each(|(_, v)| {*v = false});
    }
    pub fn mark(&self, addr: u64) {
        if !self.marks.read().unwrap().get(&addr).unwrap() {
            self.marks.write().unwrap().get_mut(&addr).map(|v| *v = true);
            self.mem.read().unwrap().get(&addr).map(|v| match v {
                HeapVal::Arr(arr) => {
                    arr.iter().for_each(|v| match v {
                        Val::Arr(v_addr) => self.mark(*v_addr),
                        Val::Str(v_addr) => self.mark(*v_addr),
                        _ => {}
                    })
                }
                _ => {}
            });
        }
    }
    fn free(&self, addr: u64) {
        self.mem.write().unwrap().remove(&addr);
        self.marks.write().unwrap().remove(&addr);
    }
    pub fn sweep(&self) {
        let mut to_free = Vec::new();
        let hidden_ref = self.hidden_ref.read().unwrap();
        self.marks.read().unwrap().iter().for_each(|(addr, marked)| {
            if !marked && !hidden_ref.contains(addr) {
                to_free.push(*addr);
            }
        });
        to_free.into_iter().for_each(|addr| self.free(addr));
    }
    pub fn add_hidden_ref(&self, addr: u64) {
        self.hidden_ref.write().unwrap().insert(addr);
    }
    pub fn rm_hidden_ref(&self, addr: u64) {
        self.hidden_ref.write().unwrap().remove(&addr);
    }
    pub fn get(&self, id: u64) -> Option<HeapVal> {
        self.mem.read().unwrap().get(&id).map(|v| (*v).clone())
    }
    pub fn get_at(&self, id: u64, idx: usize) -> Option<Val> {
        self.mem.read().unwrap().get(&id).map(|v| match v {
            HeapVal::Arr(arr) => arr[idx].clone(),
            HeapVal::Str(s) => Val::Char(s[idx]),
        })
    }
    pub fn set_arr(&self, id: u64, idx: usize, v: Val) {
        self.mem.write().unwrap().get_mut(&id).map(|arr| match arr {
            HeapVal::Arr(arr) => arr[idx] = v,
            _ => unreachable!(),
        });
    }
    pub fn push_arr(&self, id: u64, v: Val) {
        self.mem.write().unwrap().get_mut(&id).map(|arr| match arr {
            HeapVal::Arr(arr) => arr.push(v),
            _ => unreachable!(),
        });
    }
    pub fn pop_arr(&self, id: u64) -> Option<Val> {
        self.mem.write().unwrap().get_mut(&id).map(|arr| match arr {
            HeapVal::Arr(arr) => arr.pop(),
            _ => unreachable!(),
        }).unwrap_or(None)
    }
    pub fn set_str(&self, addr: u64, idx: usize, c: u8) {
        self.mem.write().unwrap().get_mut(&addr).map(|str| match str {
            HeapVal::Str(str) => str[idx] = c,
            _ => unreachable!(),
        });
    }
    pub fn push_str(&self, addr: u64, c: u8) {
        self.mem.write().unwrap().get_mut(&addr).map(|str| match str {
            HeapVal::Str(str) => str.push(c),
            _ => unreachable!(),
        });
    }
    pub fn pop_str(&self, addr: u64) -> Option<Val> {
        self.mem.write().unwrap().get_mut(&addr).map(|str| match str {
            HeapVal::Str(str) => str.pop().map(|c| Val::Char(c)),
            _ => unreachable!(),
        }).unwrap_or(None)
    }
    pub fn to_string(&self, addr: u64) -> String {
        let reader = self.mem.read().unwrap();
        let v = match reader.get(&addr) {
            Some(v) => v,
            None => return String::from("<null>"),
        };
        match v {
            HeapVal::Str(s) => {
                String::from_utf8(s.clone()).unwrap()
            }
            HeapVal::Arr(a) => {
                let s = a.iter()
                    .map(|a| match a {
                        Val::Arr(a_addr) => if addr == *a_addr {
                            String::from("<recursive>")
                        } else {
                            a.to_string(self)
                        }
                        a => a.to_string(self)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", s)
            }
        }
    }
    pub fn len(&self, id: u64) -> usize {
        let reader = self.mem.read().unwrap();
        let v = reader.get(&id).unwrap();
        match v {
            HeapVal::Str(s) => s.len(),
            HeapVal::Arr(a) => a.len(),
        }
    }
    pub fn alloc(&self, val: HeapVal) -> u64 {
        let addr = self.counter.load(SeqCst);
        self.counter.store(addr + 1, SeqCst);
        self.mem.write().unwrap().insert(addr,val);
        self.marks.write().unwrap().insert(addr, false);
        addr
    }
}
