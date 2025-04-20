use std::sync::Arc;

use crate::interpreter::Val;
use crate::heap::Heap;
use crate::environment::Env;

#[derive(Debug)]
pub struct GC {
    heap: Arc<Heap>,
}

impl<'a> GC {
    pub fn new(heap: Arc<Heap>) -> Self {
        Self { heap }
    }
    pub fn prime(&self) {
        self.heap.reset_marks();
    }
    pub fn mark(&self, e: &Env) {
        let mp = e.mp.lock().unwrap();
        mp.iter().for_each(|(_, v)| match v {
            Val::Arr(addr) => self.heap.mark(*addr),
            Val::Str(addr) => self.heap.mark(*addr),
            _ => {}
        });
        e.clear_moved_children();
        e.children.lock().unwrap().iter().for_each(|child_env| {
            self.mark(&child_env.upgrade().unwrap());
        })
    }
    pub fn sweep(&self) {
        self.heap.sweep();
    }
}
