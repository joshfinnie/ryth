use ast::{Expression, ExpressionStatement, Program};
use lexer::tokens::{ConstantKind, Token, TokenKind};
use ordered_float::OrderedFloat;

pub mod ast;

type ParseFn = fn(&mut Parser) -> Result<Expression, Error>;

#[derive(Debug)]
pub enum Error {
    NoToken,
    Expected(String),
    Syntax(String),
}

impl Error {
    pub fn expected_err(s: &str) -> Self {
        Self::Expected(s.to_string())
    }

    pub fn syntax_err(s: &str) -> Self {
        Self::Syntax(s.to_string())
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn cur_token(&self) -> TokenKind {
        self.tokens
            .get(self.pos)
            .map_or(TokenKind::Eof, |t| t.kind.clone())
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn expect_float(&mut self) -> Result<f64, Error> {
        if let TokenKind::FloatLiteral(float) = self.cur_token() {
            self.advance();
            Ok(float)
        } else {
            Err(Error::expected_err("float literal"))
        }
    }

    fn parse_float_literal(&mut self) -> Result<Expression, Error> {
        let float = self.expect_float()?;
        Ok(Expression::FloatLiteral(OrderedFloat(float)))
    }

    fn parse_string_literal(&mut self) -> Result<Expression, Error> {
        if let TokenKind::Str(s) = self.cur_token() {
            self.advance();
            Ok(Expression::Str(s))
        } else {
            Err(Error::expected_err("string literal"))
        }
    }

    fn parse_prefix_operator(
        &mut self,
        operator: ast::PrefixOperator,
    ) -> Result<Expression, Error> {
        self.advance(); // Consume the operator

        let first_expr = self.parse_expression()?;

        let second_expr = self.parse_expression()?;

        Ok(Expression::Prefix {
            operator,
            first_expr: Box::new(first_expr),
            second_expr: Box::new(second_expr),
        })
    }

    fn parse_special_chars(&mut self) -> Result<Expression, Error> {
        match self.cur_token() {
            TokenKind::Constant(ConstantKind::Zero) => {
                self.advance();
                Ok(Expression::FloatLiteral(OrderedFloat(0.)))
            }
            TokenKind::Constant(ConstantKind::Ten) => {
                self.advance();
                Ok(Expression::FloatLiteral(OrderedFloat(10.)))
            }
            TokenKind::Constant(ConstantKind::Input) => {
                self.advance();
                Ok(Expression::ReadInput(None))
            }
            TokenKind::Constant(ConstantKind::NullStr) => {
                self.advance();
                Ok(Expression::Str("".to_string()))
            }
            TokenKind::Constant(ConstantKind::Whitespace) => {
                self.advance();
                Ok(Expression::Str(" ".to_string()))
            }
            TokenKind::Constant(ConstantKind::Newline) => {
                self.advance();
                Ok(Expression::Str("\n".to_string()))
            }
            TokenKind::Constant(ConstantKind::LoopVar) => {
                self.advance();
                Ok(Expression::Ident("z".to_string()))
            }
            TokenKind::F => {
                self.advance(); // consume F

                let var_name = if let TokenKind::Ident(name) = self.cur_token() {
                    self.advance();
                    name
                } else {
                    return Err(Error::expected_err("identifier for loop variable"));
                };

                let iterable = self.parse_expression()?;

                let body = self.parse_expression()?;

                Ok(Expression::ForLoop {
                    variable: var_name,
                    iterable: Box::new(iterable),
                    body: Box::new(body),
                })
            }
            TokenKind::V => {
                self.advance();

                let range_end = self.parse_expression()?;

                let body = self.parse_expression()?;

                let iterable = Expression::Range {
                    start: Box::new(Expression::FloatLiteral(OrderedFloat(0.0))),
                    end: Box::new(range_end),
                };

                Ok(Expression::ForLoop {
                    variable: "N".to_string(),
                    iterable: Box::new(iterable),
                    body: Box::new(body),
                })
            }
            TokenKind::W => Ok(Expression::WhileLoop(0)),
            TokenKind::D => Ok(Expression::Function {
                params: vec![],
                body: vec![],
            }),
            TokenKind::I => Ok(Expression::If),
            TokenKind::R => Ok(Expression::Return),
            TokenKind::A => {
                self.advance();
                let tuple_expr = self.parse_expression()?;
                Ok(Expression::DoubleAssign(Box::new(tuple_expr)))
            }
            TokenKind::LowercaseT => Ok(Expression::Tail(0)),
            TokenKind::LowercaseJ => {
                self.advance();
                let separator = self.parse_expression()?;
                let list = self.parse_expression()?;
                Ok(Expression::Join {
                    separator: Box::new(separator),
                    list: Box::new(list),
                })
            }
            TokenKind::B => {
                self.advance();
                let expr = self.parse_expression()?;
                Ok(Expression::Bifurcate(Box::new(expr)))
            }
            TokenKind::QuestionMark => Ok(Expression::Ternary {
                var: Box::new(Expression::Str("".to_string())),
                true_: Box::new(Expression::Str("".to_string())),
                false_: Box::new(Expression::Str("".to_string())),
            }),
            TokenKind::Bang => Ok(Expression::Factorial(0)),
            TokenKind::LeftParen => {
                self.advance();
                let first = self.parse_expression()?;
                let second = self.parse_expression()?;
                Ok(Expression::Tuple(Box::new(first), Box::new(second)))
            }
            TokenKind::Plus => self.parse_prefix_operator(ast::PrefixOperator::Plus),
            TokenKind::Minus => self.parse_prefix_operator(ast::PrefixOperator::Minus),
            TokenKind::Slash => self.parse_prefix_operator(ast::PrefixOperator::Divide),
            TokenKind::Asterix => self.parse_prefix_operator(ast::PrefixOperator::Multiply),
            TokenKind::Carrot => self.parse_prefix_operator(ast::PrefixOperator::Exponent),
            TokenKind::Equal => {
                self.advance();

                if let TokenKind::Ident(var_name) = self.cur_token() {
                    self.advance();
                    let value = self.parse_expression()?;
                    Ok(Expression::Assign {
                        variable: var_name,
                        value: Box::new(value),
                    })
                } else {
                    let first = self.parse_expression()?;
                    let second = self.parse_expression()?;
                    Ok(Expression::Prefix {
                        operator: ast::PrefixOperator::Equal,
                        first_expr: Box::new(first),
                        second_expr: Box::new(second),
                    })
                }
            }
            TokenKind::Ident(name) => match name.as_str() {
                "r" => {
                    self.advance();
                    let start = self.parse_expression()?;
                    let end = self.parse_expression()?;
                    Ok(Expression::Range {
                        start: Box::new(start),
                        end: Box::new(end),
                    })
                }
                "u" | "U" => {
                    self.advance();
                    let end = self.parse_expression()?;
                    Ok(Expression::Range {
                        start: Box::new(Expression::FloatLiteral(OrderedFloat(0.0))),
                        end: Box::new(end),
                    })
                }
                "h" => {
                    self.advance();
                    let expr = self.parse_expression()?;
                    Ok(Expression::Head(Box::new(expr)))
                }
                _ => {
                    self.advance();
                    Ok(Expression::Ident(name))
                }
            },
            _ => Err(Error::expected_err("not implemented yet...")),
        }
    }

    fn parse_fn(&mut self) -> Result<ParseFn, Error> {
        let cur_tok_kind = self.cur_token();
        match cur_tok_kind {
            TokenKind::FloatLiteral(_) => Ok(Self::parse_float_literal),
            TokenKind::Constant(_) => Ok(Self::parse_special_chars),
            TokenKind::Str(_) => Ok(Self::parse_string_literal),
            TokenKind::F => Ok(Self::parse_special_chars),
            TokenKind::V => Ok(Self::parse_special_chars),
            TokenKind::W => Ok(Self::parse_special_chars),
            TokenKind::D => Ok(Self::parse_special_chars),
            TokenKind::I => Ok(Self::parse_special_chars),
            TokenKind::R => Ok(Self::parse_special_chars),
            TokenKind::A => Ok(Self::parse_special_chars),
            TokenKind::B => Ok(Self::parse_special_chars),
            TokenKind::Plus => Ok(Self::parse_special_chars),
            TokenKind::Minus => Ok(Self::parse_special_chars),
            TokenKind::Asterix => Ok(Self::parse_special_chars),
            TokenKind::Slash => Ok(Self::parse_special_chars),
            TokenKind::Carrot => Ok(Self::parse_special_chars),
            TokenKind::Equal => Ok(Self::parse_special_chars),
            TokenKind::Bang => Ok(Self::parse_special_chars),
            TokenKind::LowercaseT => Ok(Self::parse_special_chars),
            TokenKind::LowercaseJ => Ok(Self::parse_special_chars),
            TokenKind::QuestionMark => Ok(Self::parse_special_chars),
            TokenKind::LeftParen => Ok(Self::parse_special_chars),
            TokenKind::Ident(_) => Ok(Self::parse_special_chars),
            _ => Err(Error::expected_err("not implemented yet...")),
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, Error> {
        let expr = self.parse_fn()?(self)?;

        Ok(expr)
    }

    fn parse_expression_stmt(&mut self) -> Result<ExpressionStatement, Error> {
        let expr = self.parse_expression()?;
        Ok(ExpressionStatement { expr })
    }

    pub fn parse(&mut self) -> Result<Program, Error> {
        let mut stmts = vec![];
        while self.cur_token() != TokenKind::Eof {
            stmts.push(self.parse_expression_stmt()?);
            // Don't advance here - parsing functions leave us at the next token
        }

        Ok(Program(stmts))
    }
}

#[cfg(test)]
mod tests {
    use lexer::Lexer;

    use super::*;

    fn expect_ast(input: &str, ast: Program) -> Result<(), Error> {
        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer.filter_map(Result::ok).collect();
        let mut parser = Parser::new(tokens);
        let parsed = parser.parse()?;
        assert_eq!(parsed, ast);
        Ok(())
    }

    #[test]
    fn single_float() {
        expect_ast(
            "122",
            Program(vec![ExpressionStatement {
                expr: Expression::FloatLiteral(OrderedFloat(122.)),
            }]),
        )
        .unwrap();
    }

    #[test]
    fn test_special_chars() {
        expect_ast(
            "TZ",
            Program(vec![
                ExpressionStatement {
                    expr: Expression::FloatLiteral(OrderedFloat(10.)),
                },
                ExpressionStatement {
                    expr: Expression::FloatLiteral(OrderedFloat(0.)),
                },
            ]),
        )
        .unwrap();
    }
}
