use std::sync::RwLock;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering::SeqCst;
use std::collections::HashMap;

use crate::interpreter::Val;

#[derive(Clone, Debug)]
pub enum HeapVal {
    Str(Vec<u8>),
    Arr(Vec<Val>),
}

#[derive(Debug)]
pub struct Heap {
    counter: AtomicU64,
    mem: RwLock<HashMap<u64, HeapVal>>,
    marks: RwLock<HashMap<u64, bool>>,
}

impl Heap {
    pub fn new() -> Self {
        Self { counter: AtomicU64::new(0), mem: RwLock::new(HashMap::new()), marks: RwLock::new(HashMap::new()) }
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
        self.marks.read().unwrap().iter().for_each(|(addr, marked)| {
            if !marked {
                to_free.push(*addr) 
            }
        });
        to_free.into_iter().for_each(|addr| self.free(addr));
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
    pub fn set_str(&self, addr: u64, idx: usize, c: u8) {
        self.mem.write().unwrap().get_mut(&addr).map(|str| match str {
            HeapVal::Str(str) => str[idx] = c,
            _ => unreachable!(),
        });
    }
    pub fn to_string(&self, addr: u64) -> String {
        let reader = self.mem.read().unwrap();
        let v = reader.get(&addr).unwrap();
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
        self.counter.store( addr + 1, SeqCst);
        self.mem.write().unwrap().insert(addr,val);
        self.marks.write().unwrap().insert(addr, false);
        addr
    }
}
