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
    pub fn alloc(&self, val: HeapVal) -> u64 {
        let addr = self.counter.load(SeqCst);
        self.counter.store( addr + 1, SeqCst);
        self.mem.write().unwrap().insert(addr,val);
        addr
    }
}
