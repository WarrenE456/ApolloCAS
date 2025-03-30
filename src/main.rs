pub mod scanner;
pub mod parser;
pub mod error;
pub mod statement;
pub mod apollo;
pub mod interpreter;
pub mod environment;

use apollo::Apollo;

/* TODO
*
* build in functions
    * make sure that user-defined identifiers to not conflict with builtin's
* proc & types
* built in procs
* 'infinite' percision rational number representation
*
*/

fn main() {
    Apollo::run();
}
