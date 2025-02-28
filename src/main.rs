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
* fix negate/exp presidence
* expr simplification
* derivatives
* integrals
* graphing
*
*/

fn main() {
    Apollo::run();
}
