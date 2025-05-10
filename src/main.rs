pub mod apollo;
pub mod graph;
pub mod error;
pub mod scanner;
pub mod runtime;
pub mod parser;
pub mod mem;

use std::sync::{Arc, RwLock};
use std::sync::mpsc::channel;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::Duration;

use apollo::Apollo;
use runtime::Interpreter;
use mem::heap::Heap;
use graph::{Grapher, GraphSignal};
use mem::gc::GC;

/* TODO
*
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
    let (graph_tx, graph_rx) = channel::<GraphSignal>();

    let heap = Arc::new(Heap::new());
    let global = Arc::new(RwLock::new(Interpreter::new(graph_tx.clone(), heap)));
    let running = Arc::new(AtomicBool::new(true));
    
    let apollo_global = Arc::clone(&global);
    let apollo_handle = thread::spawn(move || {
        let apollo = Apollo::new(apollo_global, graph_tx);
        apollo.run();
    });

    // TODO dynamic gc's per second depending on mem usage
    let gc_global = Arc::clone(&global);
    let gc_running = Arc::clone(&running);
    let gc_handle = thread::spawn(move || {
        let gc = GC::new(Arc::clone(&gc_global.read().unwrap().heap));
        while gc_running.load(Ordering::SeqCst) {
            thread::sleep(Duration::from_millis(512));
            gc.prime();
            gc.mark(&gc_global.read().unwrap().env);
            gc.sweep();
        }
    });

    let grapher_running = Arc::clone(&running);
    let grapher_global = Arc::clone(&global);
    let grapher_handle = thread::spawn(move || {
        let mut grapher = Grapher::new(grapher_global, graph_rx);
        while grapher_running.load(Ordering::SeqCst) {
            grapher.update();
            thread::sleep(Duration::from_millis(60));
        }
    });

    apollo_handle.join().unwrap();
    running.store(false, Ordering::SeqCst);
    gc_handle.join().unwrap();
    grapher_handle.join().unwrap();
}
