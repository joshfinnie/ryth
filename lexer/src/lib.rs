use tokens::{Span, Token, TokenKind};

pub mod tokens;

#[derive(Debug, PartialEq, Eq)]
pub enum LexerError {
    UnterminatedString { span: Span },
    InvalidFloatLiteral { span: Span, value: String },
    IllegalCharacter { span: Span, character: char },
}

pub struct Lexer {
    content: String,
    pos: usize,
}

impl Lexer {
    pub fn new(prog: &str) -> Self {
        Lexer {
            content: prog.to_string(),
            pos: 0,
        }
    }

    fn advance(&mut self, dist: usize) {
        self.pos += dist;
    }

    fn peek(&self, distance: usize) -> Option<char> {
        self.content.chars().nth(self.pos + distance)
    }

    fn skip_whitespace(&mut self) {
        // We only skip standard whitespace here, as 'd' and 'b' are explicit tokens.
        while matches!(
            self.peek(0),
            Some(' ') | Some('\r') | Some('\t') | Some('\n')
        ) {
            self.advance(1);
        }
    }

    fn make_single_char_token(&mut self, kind: TokenKind) -> Token {
        let start_pos = self.pos;
        self.advance(1);
        Token::new(kind, start_pos, 1)
    }

    fn read_string(&mut self) -> Result<Token, LexerError> {
        let init_pos = self.pos;
        self.advance(1); // Consume opening quote '"'
        let content_start = self.pos;

        loop {
            match self.peek(0) {
                Some('"') => {
                    let token_size = self.pos - init_pos + 1; // Includes both quotes
                    let s = self.content[content_start..self.pos].to_string();
                    self.advance(1); // Consume closing quote '"'
                    return Ok(Token::new(TokenKind::Str(s), init_pos, token_size));
                }
                Some(_) => {
                    // NOTE: Escaping would be handled here in a real implementation
                    self.advance(1);
                }
                None => {
                    let span = Span {
                        start: init_pos,
                        end: self.pos,
                    };
                    return Err(LexerError::UnterminatedString { span });
                }
            }
        }
    }

    fn read_float(&mut self) -> Result<Token, LexerError> {
        let init_pos = self.pos;
        let mut decimal_count = 0;

        match self.peek(0) {
            Some('.') => decimal_count += 1,
            Some('0'..='9') => (),
            _ => {
                return Err(LexerError::IllegalCharacter {
                    span: Span {
                        start: self.pos,
                        end: self.pos,
                    },
                    character: self.peek(0).unwrap_or('\0'),
                });
            }
        }

        self.advance(1);

        while let Some(c) = self.peek(0) {
            match c {
                '0'..='9' => self.advance(1),
                '.' => {
                    if decimal_count >= 1 {
                        break;
                    }
                    decimal_count += 1;
                    self.advance(1);
                }
                _ => break,
            }
        }

        let float_str = self.content[init_pos..self.pos].to_string();
        let token_size = self.pos - init_pos;

        match float_str.parse::<f64>() {
            Ok(val) => Ok(Token::new(
                TokenKind::FloatLiteral(val),
                init_pos,
                token_size,
            )),
            Err(_) => Err(LexerError::InvalidFloatLiteral {
                span: Span {
                    start: init_pos,
                    end: init_pos + token_size - 1,
                },
                value: float_str,
            }),
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        use tokens::ConstantKind::*;
        use tokens::TokenKind::*;

        self.skip_whitespace();

        let Some(cur_char) = self.peek(0) else {
            return Ok(Token::new(Eof, self.pos, 0));
        };

        let token = match cur_char {
            '"' => return self.read_string(),
            '0'..='9' | '.' => return self.read_float(),

            '+' => self.make_single_char_token(Plus),
            '-' => self.make_single_char_token(Minus),
            '*' => self.make_single_char_token(Asterix),
            '/' => self.make_single_char_token(Slash),
            '^' => self.make_single_char_token(Carrot),
            '=' => self.make_single_char_token(Equal),
            '%' => self.make_single_char_token(Percent),
            '?' => self.make_single_char_token(QuestionMark),
            '!' => self.make_single_char_token(Bang),
            '(' => self.make_single_char_token(LeftParen),

            'Z' => self.make_single_char_token(Constant(Zero)),
            'T' => self.make_single_char_token(Constant(Ten)),
            'F' => self.make_single_char_token(F),
            'V' => self.make_single_char_token(V),
            'W' => self.make_single_char_token(W),
            'N' => self.make_single_char_token(Ident("N".to_string())),
            'K' => self.make_single_char_token(Ident("K".to_string())),
            'J' => self.make_single_char_token(Ident("J".to_string())),
            'G' => self.make_single_char_token(Ident("G".to_string())),
            'H' => self.make_single_char_token(Ident("H".to_string())),
            'Q' => self.make_single_char_token(Ident("Q".to_string())),
            'U' => self.make_single_char_token(Ident("U".to_string())),
            'D' => self.make_single_char_token(D),
            'I' => self.make_single_char_token(I),
            'R' => self.make_single_char_token(R),
            'A' => self.make_single_char_token(A),
            'B' => self.make_single_char_token(B),

            'z' => self.make_single_char_token(Constant(LoopVar)),
            'k' => self.make_single_char_token(Constant(NullStr)),
            'd' => self.make_single_char_token(Constant(Whitespace)),
            'b' => self.make_single_char_token(Constant(Newline)),
            'r' => self.make_single_char_token(Ident("r".to_string())),
            'u' => self.make_single_char_token(Ident("u".to_string())),
            'h' => self.make_single_char_token(Ident("h".to_string())),
            't' => self.make_single_char_token(LowercaseT),
            'j' => self.make_single_char_token(LowercaseJ),

            c @ ('a'..='y' | 'A'..='Z' | '_') => {
                self.advance(1);

                let span = Span {
                    start: self.pos,
                    end: self.pos,
                };
                self.advance(1);
                return Err(LexerError::IllegalCharacter { span, character: c });
            }

            c => {
                let span = Span {
                    start: self.pos,
                    end: self.pos,
                };
                self.advance(1);
                return Err(LexerError::IllegalCharacter { span, character: c });
            }
        };

        Ok(token)
    }
}

impl Iterator for Lexer {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(token) => {
                if token.kind == TokenKind::Eof {
                    None
                } else {
                    Some(Ok(token))
                }
            }
            Err(e) => Some(Err(e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::ConstantKind::*;

    fn get_token_kinds(input: &str) -> Result<Vec<TokenKind>, LexerError> {
        Lexer::new(input)
            .collect::<Result<Vec<Token>, LexerError>>()
            .map(|tokens| tokens.into_iter().map(|t| t.kind).collect())
    }

    #[test]
    fn test_constants_and_operators() -> Result<(), LexerError> {
        use TokenKind::*;

        let input = "Z T + Q ! ( W"; // 0, 10, +, input, !, tuple, while

        let expected = vec![
            Constant(Zero),
            Constant(Ten),
            Plus,
            Constant(Input),
            Bang,
            LeftParen,
            W,
        ];

        assert_eq!(get_token_kinds(input)?, expected);
        Ok(())
    }

    #[test]
    fn test_literals() -> Result<(), LexerError> {
        use TokenKind::*;

        let input = r#"123 4.5 "hello world" k d b"#;

        let expected = vec![
            FloatLiteral(123.0),
            FloatLiteral(4.5),
            Str("hello world".to_string()),
            Constant(NullStr),
            Constant(Whitespace),
            Constant(Newline),
        ];

        assert_eq!(get_token_kinds(input)?, expected);
        Ok(())
    }

    #[test]
    fn test_float_parsing_and_error() {
        assert_eq!(
            get_token_kinds(".5 + 10").unwrap(),
            vec![
                TokenKind::FloatLiteral(0.5),
                TokenKind::Plus,
                TokenKind::FloatLiteral(10.0)
            ]
        );

        // Invalid float: double decimal
        let err = get_token_kinds("1.2.3").unwrap_err();
        assert!(matches!(err, LexerError::InvalidFloatLiteral { .. }));

        // Unterminated string error
        let err = get_token_kinds(r#""hello"#).unwrap_err();
        assert!(matches!(err, LexerError::UnterminatedString { .. }));

        // Illegal character error
        let err = get_token_kinds("@").unwrap_err();
        assert!(matches!(
            err,
            LexerError::IllegalCharacter { character: '@', .. }
        ));
    }
}
