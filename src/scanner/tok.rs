#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokType {
    // Single characters
    Star, Slash, LParen, RParen, Carrot, Equal, Comma,
    LCurly, RCurly, LBrac, RBrac, Colon, Dollar,
    // 1-2 Characters
    Lesser, Greater, LesserEqual, GreaterEqual, Bang, BangEqual, Arrow, Minus,
    Plus, PlusPlus,
    // Fixed number of characters
    Let, Def, Or, And, If, Else, While, Break, Continue, Set, True, False, Proc, Return, In, For,
    AnyT, IntT, FloatT, FnT, BuiltInT, BoolT, UnitT, StrT, ArrT, CharT, ZT, SymAnyT, SymbolT,
    // Variable number of characters
    Float, Int, Identifier, Str, Char,
    // Msc.
    NewLine, EOF
}

#[derive(PartialEq, Eq, Clone)]
pub struct Tok {
    pub lexeme: String,
    pub line: usize,
    pub col_start: usize,
    pub col_end: usize,
    pub t: TokType,
}

use std::fmt;
impl fmt::Debug for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Tok {{ ({},{}-{}) '{}' {:?} }}", self.line, self.col_start, self.col_end, &self.lexeme, self.t)
    }
}

