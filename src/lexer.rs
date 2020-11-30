use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[allow(non_camel_case_types)]
pub enum Token {
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    ERROR,
    EOF,
    #[regex("[a-zA-Z]+", |lexer| lexer.slice().to_owned())]
    IDENT(String),
    #[regex("[0-9]+", |lexer| lexer.slice().parse())]
    INT(i32),
    #[regex(r#""[^"]*""#, |lexer| lexer.slice()[1..(lexer.slice().len()-1)].to_owned())]
    STRING(String),
    #[token("=")]
    ASSIGN,
    #[token("+")]
    PLUS,
    #[token("_")]
    MINUS,
    #[token("/")]
    SLASH,
    #[token("*")]
    ASTERISK,
    #[token("<")]
    LT,
    #[token(">")]
    GT,
    #[token("!")]
    BANG,
    #[token(",")]
    COMMA,
    #[token(";")]
    SEMICOLON,
    #[token("(")]
    LPAREN,
    #[token(")")]
    RPAREN,
    #[token("{")]
    LBRACE,
    #[token("}")]
    RBRACE,
    #[token("F")]
    FUNCTION,
    #[token("let")]
    LET,
    #[token("if")]
    IF,
    #[token("else")]
    ELSE,
    #[token("return")]
    RETURN,
    #[token("true")]
    TRUE,
    #[token("false")]
    FALSE,
    #[token("==")]
    EQ,
    #[token("!=")]
    NOT_EQ,
    #[token("Z")]
    ZERO,
    #[token("T")]
    TEN,
}

pub fn lex(input: &str) -> Vec<Token> {
    let mut tokens = Token::lexer(input).collect::<Vec<Token>>();
    tokens.push(Token::EOF);

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_tokens() {
        let input = "=+(){},;";
        let tokens = lex(input);
        assert_eq!(
            vec![
                Token::ASSIGN,
                Token::PLUS,
                Token::LPAREN,
                Token::RPAREN,
                Token::LBRACE,
                Token::RBRACE,
                Token::COMMA,
                Token::SEMICOLON,
                Token::EOF,

            ],
            tokens
        );
    }

    #[test]
    fn lex_addition_int_ten() {
        let input = "+10 T";
        let tokens = lex(input);
        assert_eq!(
            vec![
                Token::PLUS,
                Token::INT(10),
                Token::TEN,
                Token::EOF,
            ],
            tokens
        );
    }
}
