pub mod apollo;
pub mod error;
pub mod scanner;
pub mod runtime;
pub mod parser;
pub mod mem;
pub mod sym;

use std::sync::{Arc, RwLock};
use std::thread;
use std::time::Duration;

use apollo::Apollo;
use runtime::Interpreter;
use mem::heap::Heap;
use mem::gc::GC;

/* TODO FINAL LIST CAS
* 
* gcd of polynomail
* multivariable polynomial
* gcd multivariate polynomials
* rational numbers
* rational functions
* equation solving
* ln
* dirivatives
* trig functions
*
* TODO FINAL LIST PROGRAMMING LANGUAGE
*
* structs
* modules
* pipe
* ignore newlines inside parenthesis
* explicit casting
*
*/

fn main() {
    let heap = Arc::new(Heap::new());
    let global = Arc::new(RwLock::new(Interpreter::new(heap)));
    
    let gc_global = Arc::clone(&global);
    thread::spawn(move || {
        let gc = GC::new(Arc::clone(&gc_global.read().unwrap().heap));
        loop {
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
