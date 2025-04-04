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
* set statements
* true and false keywords
* continue repl on new line
* proc
* types
* graphing shit
* 'infinite' percision rational number representation
* CAS
*
*/

fn main() {
    Apollo::run();
}
