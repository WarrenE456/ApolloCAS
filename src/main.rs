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
* scope
* break
* set statements
* proc
* types
* graphing shit
* 'infinite' percision rational number representation
*
*/

fn main() {
    Apollo::run();
}
