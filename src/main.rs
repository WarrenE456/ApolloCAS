pub mod apollo;
pub mod error;
pub mod scanner;
pub mod runtime;
pub mod parser;
pub mod mem;
pub mod sym;

use std::sync::{Arc, RwLock};
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::Duration;

use apollo::Apollo;
use runtime::Interpreter;
use mem::heap::Heap;
use mem::gc::GC;

/* TODO
*
* Symbol set expressions
* blankline error
*
* range object
* proc casting
* explicit casting
* copy, type fn
* polymorphic types
*
* fix window resize issue
* fix segfault
* types
* 'infinite' percision rational number representation
* CAS
*
* Error::from
* seperate rendering from update
* fix error on blank lines
* anonymous fns
* special characters in strings
* string ops
* line aa
* let lists
* implement Error::from(msg, tok)
* <c-d> to interupt
* add comma statement
*
*/

fn main() {
    let heap = Arc::new(Heap::new());
    let global = Arc::new(RwLock::new(Interpreter::new(heap)));
    let running = Arc::new(AtomicBool::new(true));
    
    let gc_global = Arc::clone(&global);
    let gc_running = Arc::clone(&running);
    thread::spawn(move || {
        let gc = GC::new(Arc::clone(&gc_global.read().unwrap().heap));
        while gc_running.load(Ordering::SeqCst) {
            thread::sleep(Duration::from_millis(2048));
            gc.prime();
            gc.mark(&gc_global.read().unwrap().env);
            gc.sweep();
        }
    });

    let apollo_global = Arc::clone(&global);
    let apollo_handle = thread::spawn(move || {
        let apollo = Apollo::new(apollo_global);
        apollo.run();
    });

    apollo_handle.join().unwrap();
}
