use ordered_float::OrderedFloat;
use parser::ast::{Expression, ExpressionStatement, PrefixOperator, Program};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Float(f64),
    Str(String),
    Tuple(Box<Value>, Box<Value>),
    List(Vec<Value>),
    Null,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Float(n) => {
                // Format as integer if it's a whole number
                if n.fract() == 0.0 {
                    write!(f, "{}", *n as i64)
                } else {
                    write!(f, "{}", n)
                }
            }
            Value::Str(s) => write!(f, "{}", s),
            Value::Tuple(a, b) => write!(f, "({}, {})", a, b),
            Value::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
            Value::Null => write!(f, ""),
        }
    }
}

#[derive(Debug)]
pub enum InterpreterError {
    UndefinedVariable(String),
    TypeError(String),
    DivisionByZero,
    NotImplemented(String),
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterError::UndefinedVariable(var) => {
                write!(f, "Undefined variable: {}", var)
            }
            InterpreterError::TypeError(msg) => write!(f, "Type error: {}", msg),
            InterpreterError::DivisionByZero => write!(f, "Division by zero"),
            InterpreterError::NotImplemented(feature) => {
                write!(f, "Not yet implemented: {}", feature)
            }
        }
    }
}

pub struct Interpreter {
    variables: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut variables = HashMap::new();
        // Initialize N to empty string for now (will be loop variable)
        variables.insert("N".to_string(), Value::Str("".to_string()));
        // Initialize G to alphabet (as in Pyth)
        variables.insert(
            "G".to_string(),
            Value::Str("abcdefghijklmnopqrstuvwxyz".to_string()),
        );
        // Initialize H to empty list (simulating empty dict/set)
        variables.insert("H".to_string(), Value::List(vec![]));
        // Initialize Q to 10 for testing (normally would be input)
        variables.insert("Q".to_string(), Value::Float(10.0));
        Self { variables }
    }

    pub fn eval_program(&mut self, program: Program) -> Result<Vec<Value>, InterpreterError> {
        let mut results = Vec::new();
        for stmt in program.0 {
            let value = self.eval_statement(stmt)?;
            results.push(value);
        }
        Ok(results)
    }

    fn eval_statement(
        &mut self,
        stmt: ExpressionStatement,
    ) -> Result<Value, InterpreterError> {
        self.eval_expression(stmt.expr)
    }

    fn eval_expression(&mut self, expr: Expression) -> Result<Value, InterpreterError> {
        match expr {
            Expression::FloatLiteral(OrderedFloat(n)) => Ok(Value::Float(n)),
            Expression::Str(s) => Ok(Value::Str(s)),

            Expression::Ident(name) => {
                self.variables
                    .get(&name)
                    .cloned()
                    .ok_or_else(|| InterpreterError::UndefinedVariable(name))
            }

            Expression::Prefix {
                operator,
                first_expr,
                second_expr,
            } => {
                let first = self.eval_expression(*first_expr)?;
                let second = self.eval_expression(*second_expr)?;
                self.eval_prefix_operator(operator, first, second)
            }

            Expression::ReadInput(_) => {
                Err(InterpreterError::NotImplemented("ReadInput".to_string()))
            }

            Expression::Function { .. } => {
                Err(InterpreterError::NotImplemented("Function".to_string()))
            }

            Expression::ForLoop {
                variable,
                iterable,
                body,
            } => {
                // Evaluate the iterable
                let iter_val = self.eval_expression(*iterable)?;

                // Get the list of values to iterate over
                let values = match iter_val {
                    Value::List(items) => items,
                    _ => {
                        return Err(InterpreterError::TypeError(
                            "ForLoop requires an iterable (list)".to_string(),
                        ))
                    }
                };

                // Store the old value of the variable (if it exists)
                let old_value = self.variables.get(&variable).cloned();

                // Collect results from each iteration (for implicit printing)
                let mut results = Vec::new();
                for val in values {
                    // Set the loop variable
                    self.variables.insert(variable.clone(), val);

                    // Execute the body
                    let result = self.eval_expression((*body).clone())?;
                    results.push(result);
                }

                // Restore the old value of the variable
                if let Some(old) = old_value {
                    self.variables.insert(variable, old);
                } else {
                    self.variables.remove(&variable);
                }

                // Return a special list that should be printed with each element on a newline
                Ok(Value::List(results))
            }

            Expression::Range { start, end } => {
                let start_val = self.eval_expression(*start)?;
                let end_val = self.eval_expression(*end)?;

                let start_num = match start_val {
                    Value::Float(n) => n as i64,
                    _ => {
                        return Err(InterpreterError::TypeError(
                            "Range start must be a number".to_string(),
                        ))
                    }
                };

                let end_num = match end_val {
                    Value::Float(n) => n as i64,
                    _ => {
                        return Err(InterpreterError::TypeError(
                            "Range end must be a number".to_string(),
                        ))
                    }
                };

                // Create a list of numbers from start to end (exclusive)
                let values: Vec<Value> = (start_num..end_num)
                    .map(|n| Value::Float(n as f64))
                    .collect();

                Ok(Value::List(values))
            }

            Expression::WhileLoop(_) => {
                Err(InterpreterError::NotImplemented("WhileLoop".to_string()))
            }

            Expression::Ternary { .. } => {
                Err(InterpreterError::NotImplemented("Ternary".to_string()))
            }

            Expression::Factorial(_) => {
                Err(InterpreterError::NotImplemented("Factorial".to_string()))
            }

            Expression::Tail(_) => Err(InterpreterError::NotImplemented("Tail".to_string())),

            Expression::Tuple(first, second) => {
                let first_val = self.eval_expression(*first)?;
                let second_val = self.eval_expression(*second)?;
                Ok(Value::Tuple(Box::new(first_val), Box::new(second_val)))
            }

            Expression::If => Err(InterpreterError::NotImplemented("If".to_string())),

            Expression::Return => Err(InterpreterError::NotImplemented("Return".to_string())),

            Expression::Join { separator, list } => {
                let sep = self.eval_expression(*separator)?;
                let list_val = self.eval_expression(*list)?;
                self.join(sep, list_val)
            }

            Expression::Bifurcate(expr) => {
                let val = self.eval_expression(*expr)?;
                self.bifurcate(val)
            }

            Expression::Head(expr) => {
                let val = self.eval_expression(*expr)?;
                match val {
                    Value::Float(n) => Ok(Value::Float(n + 1.0)),
                    Value::List(mut items) => {
                        if items.is_empty() {
                            Err(InterpreterError::TypeError(
                                "Cannot get head of empty list".to_string(),
                            ))
                        } else {
                            Ok(items.remove(0))
                        }
                    }
                    _ => Err(InterpreterError::TypeError(
                        "Head requires a number or list".to_string(),
                    )),
                }
            }

            Expression::Assign { variable, value } => {
                let val = self.eval_expression(*value)?;
                self.variables.insert(variable, val.clone());
                Ok(val)
            }

            Expression::DoubleAssign(tuple_expr) => {
                let tuple_val = self.eval_expression(*tuple_expr)?;
                match tuple_val {
                    Value::Tuple(first, second) => {
                        self.variables.insert("G".to_string(), (*first).clone());
                        self.variables.insert("H".to_string(), (*second).clone());
                        Ok(Value::Tuple(first, second))
                    }
                    _ => Err(InterpreterError::TypeError(
                        "Double assignment requires a tuple".to_string(),
                    )),
                }
            }
        }
    }

    fn eval_prefix_operator(
        &mut self,
        operator: PrefixOperator,
        first: Value,
        second: Value,
    ) -> Result<Value, InterpreterError> {
        match operator {
            PrefixOperator::Plus => self.add(first, second),
            PrefixOperator::Minus => self.subtract(first, second),
            PrefixOperator::Multiply => self.multiply(first, second),
            PrefixOperator::Divide => self.divide(first, second),
            PrefixOperator::Exponent => self.exponent(first, second),
            PrefixOperator::Equal => self.equal(first, second),
        }
    }

    fn add(&self, first: Value, second: Value) -> Result<Value, InterpreterError> {
        match (first, second) {
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Str(a), Value::Str(b)) => Ok(Value::Str(format!("{}{}", a, b))),
            _ => Err(InterpreterError::TypeError(
                "Addition requires two numbers or two strings".to_string(),
            )),
        }
    }

    fn subtract(&self, first: Value, second: Value) -> Result<Value, InterpreterError> {
        match (first, second) {
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            _ => Err(InterpreterError::TypeError(
                "Subtraction requires two numbers".to_string(),
            )),
        }
    }

    fn multiply(&self, first: Value, second: Value) -> Result<Value, InterpreterError> {
        match (first, second) {
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            _ => Err(InterpreterError::TypeError(
                "Multiplication requires two numbers".to_string(),
            )),
        }
    }

    fn divide(&self, first: Value, second: Value) -> Result<Value, InterpreterError> {
        match (first, second) {
            (Value::Float(a), Value::Float(b)) => {
                if b == 0.0 {
                    Err(InterpreterError::DivisionByZero)
                } else {
                    Ok(Value::Float(a / b))
                }
            }
            _ => Err(InterpreterError::TypeError(
                "Division requires two numbers".to_string(),
            )),
        }
    }

    fn exponent(&self, first: Value, second: Value) -> Result<Value, InterpreterError> {
        match (first, second) {
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.powf(b))),
            _ => Err(InterpreterError::TypeError(
                "Exponentiation requires two numbers".to_string(),
            )),
        }
    }

    fn equal(&self, first: Value, second: Value) -> Result<Value, InterpreterError> {
        let result = match (first, second) {
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Null, Value::Null) => true,
            _ => false,
        };
        Ok(Value::Float(if result { 1.0 } else { 0.0 }))
    }

    fn join(&self, separator: Value, list: Value) -> Result<Value, InterpreterError> {
        let sep_str = match separator {
            Value::Str(s) => s,
            Value::Float(n) => {
                if n.fract() == 0.0 {
                    format!("{}", n as i64)
                } else {
                    format!("{}", n)
                }
            }
            _ => {
                return Err(InterpreterError::TypeError(
                    "Join separator must be a string or number".to_string(),
                ))
            }
        };

        match list {
            Value::List(items) => {
                let strings: Vec<String> = items
                    .into_iter()
                    .map(|v| match v {
                        Value::Str(s) => s,
                        Value::Float(n) => {
                            if n.fract() == 0.0 {
                                format!("{}", n as i64)
                            } else {
                                format!("{}", n)
                            }
                        }
                        _ => format!("{}", v),
                    })
                    .collect();
                Ok(Value::Str(strings.join(&sep_str)))
            }
            _ => Err(InterpreterError::TypeError(
                "Join requires a list as second argument".to_string(),
            )),
        }
    }

    fn bifurcate(&self, val: Value) -> Result<Value, InterpreterError> {
        // Bifurcate creates [val, quoted_val]
        // For a quine, this adds a leading quote to reconstruct string literals
        match val {
            Value::Str(s) => {
                let original = Value::Str(s.clone());
                let quoted = Value::Str(format!("\"{}\"", s));
                Ok(Value::List(vec![original, quoted]))
            }
            Value::Float(n) => {
                let original = Value::Float(n);
                let quoted_str = if n.fract() == 0.0 {
                    format!("\"{}\"", n as i64)
                } else {
                    format!("\"{}\"", n)
                };
                Ok(Value::List(vec![original, Value::Str(quoted_str)]))
            }
            _ => Err(InterpreterError::TypeError(
                "Bifurcate not implemented for this type".to_string(),
            )),
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_float_literal() {
        let mut interp = Interpreter::new();
        let program = Program(vec![ExpressionStatement {
            expr: Expression::FloatLiteral(OrderedFloat(42.0)),
        }]);

        let results = interp.eval_program(program).unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0], Value::Float(42.0));
    }

    #[test]
    fn test_string_literal() {
        let mut interp = Interpreter::new();
        let program = Program(vec![ExpressionStatement {
            expr: Expression::Str("hello world".to_string()),
        }]);

        let results = interp.eval_program(program).unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0], Value::Str("hello world".to_string()));
    }

    #[test]
    fn test_addition() {
        let mut interp = Interpreter::new();
        let program = Program(vec![ExpressionStatement {
            expr: Expression::Prefix {
                operator: PrefixOperator::Plus,
                first_expr: Box::new(Expression::FloatLiteral(OrderedFloat(5.0))),
                second_expr: Box::new(Expression::FloatLiteral(OrderedFloat(10.0))),
            },
        }]);

        let results = interp.eval_program(program).unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0], Value::Float(15.0));
    }

    #[test]
    fn test_multiplication() {
        let mut interp = Interpreter::new();
        let program = Program(vec![ExpressionStatement {
            expr: Expression::Prefix {
                operator: PrefixOperator::Multiply,
                first_expr: Box::new(Expression::FloatLiteral(OrderedFloat(3.0))),
                second_expr: Box::new(Expression::FloatLiteral(OrderedFloat(7.0))),
            },
        }]);

        let results = interp.eval_program(program).unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0], Value::Float(21.0));
    }
}
