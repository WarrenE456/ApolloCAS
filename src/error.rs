#[derive(Debug, Clone)]
pub enum Special {
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct Error {
    pub msg: String,
    pub line: usize,
    pub col_start: usize,
    pub col_end: usize,
    pub special: Option<Special>
}

impl Error {
    pub fn display(&self, lines: &Vec<String>) {
        println!("Error on line {}: {}", self.line, self.msg);
        println!("{}", lines[self.line - 1].trim_end());
        if self.col_start == self.col_end {
            println!("{}^", "-".repeat(self.col_start - 1));
        } else {
            println!("{}{}", " ".repeat(self.col_start - 1), "~".repeat(self.col_end - self.col_start + 1));
        }
    }
}
