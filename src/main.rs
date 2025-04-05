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
* strings
* graphing shit
* types
* 'infinite' percision rational number representation
* CAS
*
* anonymous fns
* special characters in strings
* string ops
*
*/

fn main() {
    Apollo::run();
}
