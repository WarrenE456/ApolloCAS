pub mod scanner;
pub mod parser;
pub mod error;
pub mod statement;
pub mod apollo;
pub mod interpreter;
pub mod environment;
pub mod graph;

use apollo::Apollo;

/* TODO
*
* graphing shit
* types
* 'infinite' percision rational number representation
* CAS
*
* anonymous fns
* special characters in strings
* string ops
* line aa
*
*/

fn main() {
    Apollo::run();
}
