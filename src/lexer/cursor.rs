use std::str::Chars;

/// Peekable iterator over a char sequence
///
/// Next character can be peeked via 'peek' method
/// and position can be shifted by 'advance' method
///
/// Pulled from rustc_lexer crate

#[allow(dead_code)]
pub struct Cursor<'a> {
    len_remaining: usize,
    // Iterator over chars, simpler than &str and is slightly faster
    chars: Chars<'a>,
}

pub(crate) const EOF: char = '\0';

#[allow(dead_code)]
impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            len_remaining: input.len(),
            chars: input.chars(),
        }
    }

    pub fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }

    /// Peek at next charcter to be consumed
    pub fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF)
    }

    /// Peek two characters ahead
    pub fn peek_next(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF)
    }

    /// Checks if there are more characters to be consumed
    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    /// Consume next character
    pub fn advance(&mut self) -> Option<char> {
        let c = self.chars.next()?;

        Some(c)
    }

    /// Consume characters while some predicate is true or until EOF is reached
    pub fn advance_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.peek()) && !self.is_eof() {
            self.advance();
        }
    }
}
