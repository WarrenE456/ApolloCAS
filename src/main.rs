pub mod scanner;
pub mod parser;
pub mod error;
pub mod statement;
pub mod apollo;
pub mod interpreter;
pub mod environment;
pub mod graph;

use std::sync::{Arc, RwLock};
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::Duration;

use apollo::Apollo;
use interpreter::Interpreter;
use graph::Grapher;

/* TODO
*
* types
* 'infinite' percision rational number representation
* CAS
*
* fix error on blank lines
* anonymous fns
* special characters in strings
* string ops
* line aa
* implement Error::from(msg, tok)
* <c-d> to interupt
*
*/

fn main() {
    let global = Arc::new(RwLock::new(Interpreter::new()));

    let apollo_global = Arc::clone(&global);
    let apollo_handle = thread::spawn(move || {
        let apollo = Apollo::from(apollo_global);
        apollo.run();
    });

    let running = Arc::new(AtomicBool::new(true));

    let grapher_running = Arc::clone(&running);
    let grapher_global = Arc::clone(&global);
    thread::spawn(move || {
        thread::sleep(Duration::from_millis(5000));
        let mut grapher = Grapher::new(grapher_global);
        while grapher_running.load(Ordering::SeqCst) {
            grapher.update();
            thread::sleep(Duration::from_millis(60));
        }
    });

    apollo_handle.join().unwrap();
    running.store(false, Ordering::SeqCst);
}
