use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Expression(Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Const(i32),
    String(String),
    Boolean(bool),
    Ident(String),
    Prefix{prefix: Prefix, value: Box<Expr>},
    Infix{operator: Operator, left: Box<Expr>, right: Box<Expr>},
    If{condition: Box<Expr>, consequence: Vec<Statement>, alternative: Vec<Statement>},
    Function{parameters: Vec<String>, body: Vec<Statement>},
    Call{function: Box<Expr>, arguments: Vec<Expr>},
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
    Bang,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    Equals,
    NotEquals,
}

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Difference,
    Product,
    Prefix,
}

pub fn parse(input: &mut Vec<Token>) -> Vec<Statement> {
	let mut program = vec![];

    loop {
        let token = &input[0];

        match token {
            Token::EOF => break,
            Token::RBRACE => break,
            _ => program.push(
                Statement::Expression(
                    parse_expression(input, Precedence::Lowest)
                )
            )
        }

        assert_eq!(Token::SEMICOLON, input.remove(0));
    }

    program
}

fn parse_expression(input: &mut Vec<Token>, precedence: Precedence) -> Expr {
    let mut left_expr = match input.remove(0) {
        Token::INT(value) => Expr::Const(value),
        Token::TRUE => Expr::Boolean(true),
        Token::FALSE => Expr::Boolean(false),
        Token::TEN => Expr::Const(10),
        Token::ZERO => Expr::Const(0),
        Token::IDENT(value) => {
            if &input[0] == &Token::LPAREN {
                input.remove(0);
                let mut args = vec![];
                loop {
                    match &input[0] {
                        Token::RPAREN => {
                            input.remove(0);
                            break
                        },
                        _ => {
                            args.push(parse_expression(input, Precedence::Lowest));
                        },
                   }

                   match input.remove(0) {
                       Token::RPAREN => break,
                       Token::COMMA => continue,
                       _ => panic!("Unexpected parameter found while parsing function args."),
                   }
                }
                Expr::Call {
                    function: Box::new(Expr::Ident(value)),
                    arguments: args
                }
            } else {
                Expr::Ident(value)
            }
        },
        Token::BANG => Expr::Prefix{
            prefix: Prefix::Bang,
            value: Box::new(parse_expression(input, Precedence::Prefix))
        },
        Token::MINUS => Expr::Prefix{
            prefix: Operator::Minus,
            value: Box::new(parse_expression(input, Precedence::Difference))
        },
        Token::PLUS => Expr::Prefix{
            prefix: Operator::Plus,
            value: Box::new(parse_expression(input, Precedence::Sum))
        },
        Token::LPAREN => {
            let expr = parse_expression(input, Precedence::Lowest);
            assert_eq!(Token::RPAREN, input.remove(0));

            expr
        },
        Token::IF => {
            assert_eq!(Token::LPAREN, input.remove(0));
            let condition = parse_expression(input, Precedence::Lowest);
            assert_eq!(Token::RPAREN, input.remove(0));

            assert_eq!(Token::LBRACE, input.remove(0));
            let consequence = parse(input);
            assert_eq!(Token::RBRACE, input.remove(0));

            let alternative = if &input[0] == &Token::ELSE {
                input.remove(0);

                assert_eq!(Token::LBRACE, input.remove(0));
                let alternative = parse(input);
                assert_eq!(Token::RBRACE, input.remove(0));

                alternative
            } else {
                Vec::new()
            };

            Expr::If {
                condition: Box::new(condition),
                consequence,
                alternative,
            }
        },
        Token::FUNCTION => {
            let mut parameters = vec![];
            assert_eq!(Token::LPAREN, input.remove(0));
            // must be idents seperated by comma, or RPAREN
            loop {
                match input.remove(0) {
                    Token::RPAREN => break,
                    Token::IDENT(ident) => {
                        parameters.push(ident);
                        match input.remove(0) {
                            Token::RPAREN => break,
                            Token::COMMA => continue,
                            _ => panic!("unexpected parameter found while parsing function parameters"),
                        }
                    },
                    _ => panic!("unexpected parameter found while parsing function parameters"),
                }
            }

            assert_eq!(Token::LBRACE, input.remove(0));
            let body = parse(input);
            assert_eq!(Token::RBRACE, input.remove(0));

            Expr::Function {
                parameters,
                body,
            }
        },
        Token::STRING(string) => Expr::String(string),
        _ => panic!("parse error at expression"),
    };

    let mut next_token = &input[0];
    while precedence < next_token.precedence() {
        left_expr = parse_infix(left_expr, input);
        next_token = &input[0];
    }

    left_expr
}

fn parse_infix(left: Expr, input: &mut Vec<Token>) -> Expr {
    let next_token = input.remove(0);
    let operator = match &next_token {
        Token::PLUS => Operator::Plus,
        Token::MINUS => Operator::Minus,
        Token::SLASH => Operator::Divide,
        Token::ASTERISK => Operator::Multiply,
        Token::LT => Operator::LessThan,
        Token::GT => Operator::GreaterThan,
        Token::EQ => Operator::Equals,
        Token::NOT_EQ => Operator::NotEquals,
        _ => panic!("parse infix called on invalid operator"),
    };
    Expr::Infix {
        left: Box::new(left),
        operator,
        right: Box::new(parse_expression(input, next_token.precedence())),
    }
}

impl Token {
    fn precedence(&self) -> Precedence {
        match self {
            Token::PLUS => Precedence::Sum,
            Token::MINUS => Precedence::Sum,
            Token::SLASH => Precedence::Product,
            Token::ASTERISK => Precedence::Product,
            Token::LT => Precedence::LessGreater,
            Token::GT => Precedence::LessGreater,
            Token::EQ => Precedence::Equals,
            Token::NOT_EQ => Precedence::Equals,
            _ => Precedence::Lowest
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;

    #[test]
    fn parse_expression_statement_const() {
        let input = "5;";
        let mut tokens = lex(input);
        let ast = parse(&mut tokens);

        assert_eq!(
            vec![
                Statement::Expression(Expr::Const(5)),
            ],
            ast
        );
    }

    #[test]
    fn parse_expression_statement_addition() {
        let input = "+5 T;";
        let mut tokens = lex(input);
        let ast = parse(&mut tokens);

        assert_eq!(
            vec![
                Statement::Expression(Expr::Const(5)),
            ],
            ast
        );
    }
}
