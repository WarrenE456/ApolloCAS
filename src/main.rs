pub mod scanner;
pub mod parser;
pub mod error;
pub mod line;
pub mod apollo;
pub mod interpreter;
pub mod environment;

use apollo::Apollo;

// TODO groups, negation, and exponents

fn main() {
    Apollo::run();
}
