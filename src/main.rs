pub mod scanner;
pub mod parser;
pub mod error;
pub mod statement;
pub mod apollo;
pub mod interpreter;
pub mod environment;

use apollo::Apollo;

// TODO exponents

fn main() {
    Apollo::run();
}
