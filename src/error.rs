#[derive(Debug)]
pub struct Error {
    pub msg: String,
    pub line: usize,
    pub col: usize
}

