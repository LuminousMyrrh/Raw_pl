use crate::scan::token::{Token, TokenType, TokenType::*};
use crate::utils::utils::threw_scan_error;

pub struct Scanner {
    source: String,
    start: usize,
    current: usize,
    line: usize,
    tokens: Vec<Token>,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {
            source,
            start: 0,
            current: 0,
            line: 1,
            tokens: Vec::new(),
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(Eof, String::from("")));
        self.tokens.clone()
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(LParen),
            ')' => self.add_token(RParen),
            '{' => self.add_token(LBrace),
            '}' => self.add_token(RBrace),
            ',' => self.add_token(Comma),
            '.' => self.add_token(Dot),
            '-' => self.add_token(Minus),
            '+' => self.add_token(Plus),
            ';' => self.add_token(Semicolon),
            '*' => self.add_token(Star),
            '!' => {
                if self.match_token('=') {
                    self.add_token(BangEqual);
                } else {
                    self.add_token(Bang);
                }
            }
            '=' => {
                if self.match_token('=') {
                    self.add_token(EqualEqual);
                } else {
                    self.add_token(Equal);
                }
            }
            '<' => {
                if self.match_token('=') {
                    self.add_token(LessEqual);
                } else {
                    self.add_token(Less);
                }
            }
            '>' => {
                if self.match_token('=') {
                    self.add_token(GreaterEqual);
                } else {
                    self.add_token(Greater);
                }
            }
            '/' => {
                if self.match_token('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(Slash);
                }
            }
            '"' => self.found_string(),
            ' ' | '\r' | '\t' => {}
            '\n' => self.line += 1,
            _ => {
                if c.is_numeric() {
                    self.found_number();
                } else if self.is_alpha(c) {
                    self.found_identifier();
                } else {
                    threw_scan_error(c, self.line);
                }
            }
        }
    }

    fn is_alpha(&self, c: char) -> bool {
        c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_'
    }

    fn match_token(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source.chars().nth(self.current) != Some(expected) {
            return false;
        }
        self.current += 1;
        true
    }

    fn add_token(&mut self, t_type: TokenType) {
        let text = &self.source[self.start..self.current];
        self.tokens.push(Token::new(t_type, text.to_string()));
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn found_string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            threw_scan_error('"', self.line);
            return;
        }

        self.advance();
        self.add_token(RawStr);
    }

    fn found_number(&mut self) {
        while self.peek().is_numeric() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_numeric() {
            self.advance();
            while self.peek().is_numeric() {
                self.advance();
            }
            let value = &self.source[self.start..self.current];
            match value.parse::<f64>() {
                Ok(_) => self.add_token(FloatNum),
                Err(_) => threw_scan_error('.', self.line),
            }
        } else {
            let value = &self.source[self.start..self.current];
            match value.parse::<i64>() {
                Ok(_) => self.add_token(IntNum),
                Err(_) => threw_scan_error('.', self.line),
            }
        }
    }

    fn found_identifier(&mut self) {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        match text {
            "and" => self.add_token(And),
            "else" => self.add_token(Else),
            "false" => self.add_token(False),
            "through" => self.add_token(Through),
            "oper" => self.add_token(Oper),
            "if" => self.add_token(If),
            "nil" => self.add_token(Nil),
            "or" => self.add_token(Or),
            "return" => self.add_token(Repay),
            "super" => self.add_token(Super),
            "this" => self.add_token(This),
            "true" => self.add_token(True),
            "var" => self.add_token(Var),
            "prolonging" => self.add_token(Prolonging),
            _ => self.add_token(Ident),
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source.chars().nth(self.current).unwrap()
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        self.source.chars().nth(self.current + 1).unwrap()
    }
}
