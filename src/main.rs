pub mod scanner;
pub mod parser;
pub mod error;
pub mod line;
pub mod apollo;
pub mod interpreter;

use apollo::Apollo;

fn main() {
    Apollo::run();
}
