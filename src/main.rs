pub mod scanner;
pub mod parser;
pub mod error;
pub mod statement;
pub mod apollo;
pub mod interpreter;
pub mod environment;

use apollo::Apollo;


// TODO fix errors in functions and make functions only work on identifiers instead of expresions

fn main() {
    Apollo::run();
}
