use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub t_type: TokenType,
    //pub literal: Option<Value>,
    pub value: Option<String>,
}

impl Token {
    pub fn new(t_type: TokenType, value: Option<String>) -> Self {
        Token { t_type, value }
    }

    pub fn _to_str(&self) -> String {
        format!("{:#?} {}", self.t_type, self.value.clone().unwrap())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Ident,
    RawStr,
    IntNum,
    FloatNum,
    // Keywords.
    And,
    Else,
    False,
    Oper,
    Through,
    If,
    Nil,
    Or,
    Repay,
    Super,
    This,
    True,
    Var,
    Prolonging,
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Token: {:?} ", self.t_type)?;
        if let Some(value) = &self.value {
            write!(f, "Value: {}", value)?;
        }
        Ok(())
    }
}
