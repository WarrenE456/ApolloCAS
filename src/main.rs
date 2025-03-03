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
* seperate numbers from other values
* factorial (higher precidence than factorial)
* 'infinite' percision rational number representation
* gcd/misc. functions
*
*/

fn main() {
    Apollo::run();
}
