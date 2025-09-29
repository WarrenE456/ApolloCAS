use std::sync::RwLock;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering::SeqCst;
use std::sync::Arc;
use std::collections::{HashMap, HashSet};

use crate::runtime::val::{Val, Num, FnVal, Type, SymT};
use crate::sym::SymExpr;
use crate::parser::expr::Call;
use crate::runtime::Interpreter;
use crate::error::Error;

#[derive(Clone, Debug)]
pub enum HeapVal {
    Str(Vec<u8>),
    Arr(Vec<Val>),
    Iter(Iter),
    Sym(SymExpr),
    Fn(Arc<FnVal>),
}

#[derive(Clone, Debug)]
pub enum Iter {
    Range(Range),
    Heap(HeapIter),
}

impl Iter {
    pub fn next(&self, heap: &Heap) -> Option<Val> {
        match self {
            Iter::Range(r) => r.next(),
            Iter::Heap(h) => h.next(heap),
        }
    }
    pub fn len(&self, heap: &Heap) -> usize {
        match self {
            Iter::Range(r) => (r.stop - r.start + 1).try_into().unwrap(),
            Iter::Heap(h) => heap.len(h.addr),
        }
    }
}

#[derive(Debug)]
pub struct Range {
    i: RwLock<i64>,
    start: i64,
    stop: i64,
    inc: i64,
}

impl Clone for Range {
    fn clone(&self) -> Self {
        Range { i: RwLock::new(self.i.read().unwrap().clone()), start: self.start, stop: self.stop, inc: self.inc }
    }
}

impl Range {
    pub fn new(start: i64, stop: i64, inc: i64) -> Range {
        Range { i: RwLock::new(start), start, stop, inc }
    }
    pub fn next(&self) -> Option<Val> {
        let mut i = self.i.write().unwrap();
        if (self.inc > 0 && *i < self.stop) || (self.inc < 0 && *i > self.stop)  {
            let next = Val::Num(Num::Int(*i));
            *i += self.inc;
            Some(next)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct HeapIter {
    pub addr: u64,
    pub idx: RwLock<usize>,
}

impl Clone for HeapIter {
    fn clone(&self) -> Self {
        HeapIter { idx: RwLock::new(self.idx.read().unwrap().clone()), addr: self.addr }
    }
}

impl HeapIter {
    pub fn new(addr: u64) -> Self{
        Self { addr, idx: RwLock::new(0) }
    }
    pub fn next(&self, h: &Heap) -> Option<Val> {
        let mut idx = self.idx.write().unwrap();
        if *idx < h.len(self.addr) {
            *idx += 1;
            Some(h.get_at(self.addr, *idx - 1)?)
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
            self.mem.try_read().unwrap().get(&addr).map(|v| match v {
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
        self.mem.try_write().unwrap().remove(&addr);
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
    pub fn add_pin(&self, addr: u64) {
        self.hidden_ref.write().unwrap().insert(addr);
    }
    pub fn rm_pin(&self, addr: u64) {
        self.hidden_ref.write().unwrap().remove(&addr);
    }
    pub fn get(&self, id: u64) -> Option<HeapVal> {
        self.mem.try_read().unwrap().get(&id).map(|v| (*v).clone())
    }
    pub fn get_at(&self, id: u64, idx: usize) -> Option<Val> {
        self.mem.try_read().unwrap().get(&id).map(|v| match v {
            HeapVal::Arr(arr) => Some(arr[idx].clone()),
            HeapVal::Str(s) => Some(Val::Char(s[idx])),
            _ => None
        }).flatten()
    }
    pub fn set_arr(&self, id: u64, idx: usize, v: Val) {
        self.mem.try_write().unwrap().get_mut(&id).map(|arr| match arr {
            HeapVal::Arr(arr) => arr[idx] = v,
            _ => panic!("Cannot call set_arr on non-arr"),
        });
    }
    pub fn push_arr(&self, id: u64, v: Val) {
        self.mem.try_write().unwrap().get_mut(&id).map(|arr| match arr {
            HeapVal::Arr(arr) => arr.push(v),
            _ => panic!("Cannot call push_arr on non-arr"),
        });
    }
    pub fn pop_arr(&self, id: u64) -> Option<Val> {
        self.mem.try_write().unwrap().get_mut(&id).map(|arr| match arr {
            HeapVal::Arr(arr) => arr.pop(),
            _ => panic!("Cannot call pop_arr on non-arr"),
        }).unwrap_or(None)
    }
    pub fn set_str(&self, addr: u64, idx: usize, c: u8) {
        self.mem.try_write().unwrap().get_mut(&addr).map(|str| match str {
            HeapVal::Str(str) => str[idx] = c,
            _ => panic!("Cannot call set_str on non-string"),
        });
    }
    pub fn push_str(&self, addr: u64, c: u8) {
        self.mem.try_write().unwrap().get_mut(&addr).map(|str| match str {
            HeapVal::Str(str) => str.push(c),
            _ => panic!("Cannot call push_str on non-string"),
        });
    }
    pub fn pop_str(&self, addr: u64) -> Option<Val> {
        self.mem.try_write().unwrap().get_mut(&addr).map(|str| match str {
            HeapVal::Str(str) => str.pop().map(|c| Val::Char(c)),
            _ => panic!("Cannot call pop_str on non-string"),
        }).unwrap_or(None)
    }
    pub fn next_iter(&self, addr: u64) -> Option<Val> {
        self.mem.try_read().unwrap().get(&addr).map(|v| match v {
            HeapVal::Iter(iter) => Some(iter.next(self)),
            _ => None,
        }).flatten().unwrap_or(None)
    }
    pub fn simplify_sym(&self, addr: u64) {

        let mut mem = self.mem.try_write().unwrap();
        let sym = mem.get_mut(&addr).unwrap();
        match sym {
            HeapVal::Sym(s) => {
                *s = std::mem::take(s).simplify();
            }
            _ => panic!("Cannot call simplify_sym on non-sym"),
        }
    }
    pub fn to_string(&self, addr: u64) -> String {
        let reader = self.mem.try_read().unwrap();
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
            HeapVal::Sym(s) => s.to_string(),
            HeapVal::Fn(_) | HeapVal::Iter(_) => unreachable!(),
        }
    }
    pub fn len(&self, id: u64) -> usize {
        let reader = self.mem.try_read().unwrap();
        let v = reader.get(&id).unwrap();
        match v {
            HeapVal::Str(s) => s.len(),
            HeapVal::Arr(a) => a.len(),
            HeapVal::Iter(iter) => iter.len(self),
            HeapVal::Fn(f) => f.param_count(),
            HeapVal::Sym(_) => todo!(),
        }
    }
    pub fn gcd(&self, a: u64, b: u64) -> Result<SymExpr, String> {
        let reader = self.mem.read().unwrap();
        let a = match reader.get(&a).unwrap() {
            HeapVal::Sym(s) => s,
            _ => unreachable!(),
        };
        let b = match reader.get(&b).unwrap() {
            HeapVal::Sym(s) => s,
            _ => unreachable!(),
        };
        a.gcd(b)
    }
    pub fn call(&self, id: u64, c: &Call, i: &Interpreter) -> Result<Val, Error> {
        let f = {
            let reader = self.mem.try_read().unwrap();
            let v = reader.get(&id).unwrap();
            match v {
                HeapVal::Fn(f) => Arc::clone(f),
                _ => unreachable!()
            }
        };
        f.call(c, i)
    }
    pub fn type_fn(&self, id: u64) -> Type {
        let reader = self.mem.try_read().unwrap();
        let v = reader.get(&id).unwrap();
        match v {
            HeapVal::Fn(f) => {
                let param_t: Vec<Type> = f.params.iter().map(|(_, t)| t.clone()).collect();
                Type::Fn(param_t, Box::new(f.return_t.clone()))
            }
            _ => unreachable!()
        }
    }
    pub fn type_sym(&self, id: u64) -> SymT {
        let reader = self.mem.try_read().unwrap();
        let v = reader.get(&id).unwrap();
        match v {
            HeapVal::Sym(s) => match s {
                SymExpr::Z(_) => SymT::Z,
                SymExpr::Symbol(_) => SymT::Symbol,
                SymExpr::Polynomial(p) => SymT::Polynomial(p.var.clone()),
                _ => SymT::Any,
            },
            _ => unreachable!()
        }
    }
    pub fn alloc(&self, val: HeapVal) -> u64 {
        let addr = self.counter.load(SeqCst);
        self.counter.store(addr + 1, SeqCst);
        self.mem.try_write().unwrap().insert(addr,val);
        self.marks.write().unwrap().insert(addr, false);
        addr
    }
}

pub struct HeapPin<'a> {
    addr: u64,
    h: &'a Heap,
}

impl<'a> HeapPin<'a> {
    pub fn new(addr: u64, h: &'a Heap) -> HeapPin<'a> {
        h.add_pin(addr);
        HeapPin { addr, h }
    }
}

impl<'a> Drop for HeapPin<'a> {
    fn drop(&mut self) {
        self.h.rm_pin(self.addr)
    }
}
