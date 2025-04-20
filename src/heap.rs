use std::sync::RwLock;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering::SeqCst;
use std::collections::HashMap;

use crate::interpreter::Val;

#[derive(Clone)]
pub enum HeapVal {
    Str(Vec<u8>),
    Arr(Vec<Val>),
}

pub struct Heap {
    counter: AtomicU64,
    mem: RwLock<HashMap<u64, HeapVal>>,
}

impl Heap {
    pub fn new() -> Self {
        Self { counter: AtomicU64::new(0), mem: RwLock::new(HashMap::new()) }
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
    pub fn set_str(&self, id: u64, idx: usize, c: u8) {
        self.mem.write().unwrap().get_mut(&id).map(|str| match str {
            HeapVal::Str(str) => str[idx] = c,
            _ => unreachable!(),
        });
    }
    pub fn to_string(&self, id: u64) -> String {
        let reader = self.mem.read().unwrap();
        let v = reader.get(&id).unwrap();
        match v {
            HeapVal::Str(s) => {
                String::from_utf8(s.clone()).unwrap()
            }
            HeapVal::Arr(a) => {
                let s = a.iter()
                    .map(|a| a.to_string(self))
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
        addr
    }
}
