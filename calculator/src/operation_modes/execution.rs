use std::io::{BufRead, Write};
use std::ops::Neg;

use crate::operation_modes::error::CalculatorError;
use crate::operation_modes::error::CalculatorErrorType::{
    EmptyDataStack, IOError, NotEnoughElementsOnTheDataStack, WrongOperationMode,
};
use crate::operation_modes::{CalculatorResult, OperationMode};
use crate::{value::Value, Calculator};

/// This mode is responsible for the execution of commands.
#[derive(Default)]
pub(crate) struct ExecutionMode {}

impl<R: BufRead, W: Write> OperationMode<R, W> for ExecutionMode {
    fn read(&self, ctx: &mut Calculator<R, W>, input: char) -> CalculatorResult<()> {
        if ctx.operation_mode < -1 {
            return Err(CalculatorError::from_type(WrongOperationMode(
                ctx.operation_mode,
            )));
        }

        match input {
            '0'..='9' => {
                // Whole number creation mode
                let input_value = input.to_digit(10).unwrap();
                ctx.stack.push(Value::Single(input_value as f64));
                ctx.operation_mode = -1;
                Ok(())
            }
            '.' => {
                // Decimal place construction mode
                ctx.stack.push(Value::Single(0.0f64));
                ctx.operation_mode = -2;
                Ok(())
            }
            '(' => {
                // List construction mode
                ctx.stack.push(Value::List("".to_string()));
                ctx.operation_mode = 1;
                Ok(())
            }
            'a'..='z' => {
                // Load from register
                let register_index = (input as u32 - 'a' as u32) as usize;
                let register_data = ctx.register_set[register_index].clone();
                ctx.stack.push(register_data);
                Ok(())
            }
            'A'..='Z' => {
                // Store in register
                let register_index = (input as u32 - 'A' as u32) as usize;
                if let Some(stack_value) = ctx.stack.pop() {
                    ctx.register_set[register_index] = stack_value;
                    Ok(())
                } else {
                    Err(CalculatorError::from_type(EmptyDataStack))
                }
            }
            '=' | '<' | '>' => {
                // Comparison
                let (b, a) = (ctx.stack.pop(), ctx.stack.pop());

                let result = match (input, &a, &b) {
                    ('=', Some(Value::Single(a)), Some(Value::Single(b))) => {
                        let r = (a - b).abs() < ctx.epsilon;
                        Value::Single(r as i8 as f64)
                    }
                    ('=', Some(Value::List(a)), Some(Value::List(b))) => {
                        Value::Single((a == b) as i8 as f64)
                    }
                    ('=', Some(Value::Single(_)), Some(Value::List(_))) => {
                        Value::Single(false as i8 as f64)
                    }
                    ('=', Some(Value::List(_)), Some(Value::Single(_))) => {
                        Value::Single(false as i8 as f64)
                    }
                    ('<', Some(Value::Single(a)), Some(Value::Single(b))) => {
                        Value::Single((a < b) as i8 as f64)
                    }
                    ('<', Some(Value::List(a)), Some(Value::List(b))) => {
                        Value::Single((a < b) as i8 as f64)
                    }
                    ('<', Some(Value::Single(_)), Some(Value::List(_))) => {
                        Value::Single(true as i8 as f64)
                    }
                    ('<', Some(Value::List(_)), Some(Value::Single(_))) => {
                        Value::Single(false as i8 as f64)
                    }
                    ('>', Some(Value::Single(a)), Some(Value::Single(b))) => {
                        Value::Single((a > b) as i8 as f64)
                    }
                    ('>', Some(Value::List(a)), Some(Value::List(b))) => {
                        Value::Single((a > b) as i8 as f64)
                    }
                    ('>', Some(Value::Single(_)), Some(Value::List(_))) => {
                        Value::Single(false as i8 as f64)
                    }
                    ('>', Some(Value::List(_)), Some(Value::Single(_))) => {
                        Value::Single(true as i8 as f64)
                    }
                    (_, Some(_a), None) => {
                        // Push '_a' back on the stack?
                        return Err(CalculatorError::from_type(NotEnoughElementsOnTheDataStack));
                    }
                    (_, None, None) => return Err(CalculatorError::from_type(EmptyDataStack)),
                    _ => Value::Single(false as i8 as f64),
                };
                ctx.stack.push(result);
                Ok(())
            }
            '?' => {
                // Check
                if let Some(value) = ctx.stack.pop() {
                    let comparison = match value {
                        Value::Single(_) => (!value.to_bool(ctx.epsilon)).into(),
                        Value::List(s) => {
                            if s == "()" {
                                true.into()
                            } else {
                                false.into()
                            }
                        }
                    };
                    ctx.stack.push(comparison);
                    Ok(())
                } else {
                    Err(CalculatorError::from_type(NotEnoughElementsOnTheDataStack))
                }
            }
            '+' | '-' | '*' | '/' | '&' | '|' => {
                // Arithmetic or logic operation

                let (b, a) = (ctx.stack.pop(), ctx.stack.pop());
                let result = match (input, &a, &b) {
                    ('+', Some(Value::Single(c)), Some(Value::Single(d))) => Value::Single(c + d),
                    ('-', Some(Value::Single(c)), Some(Value::Single(d))) => Value::Single(c - d),
                    ('*', Some(Value::Single(c)), Some(Value::Single(d))) => Value::Single(c * d),
                    ('/', Some(Value::Single(c)), Some(Value::Single(d))) => Value::Single(c / d),
                    ('&', Some(Value::Single(_)), Some(Value::Single(_))) => {
                        // We can use unwrap here, because it is already checked in the match arm
                        (a.unwrap().to_bool(ctx.epsilon) & b.unwrap().to_bool(ctx.epsilon)).into()
                    }
                    ('|', Some(Value::Single(_)), Some(Value::Single(_))) => {
                        // We can use unwrap here, because it is already checked in the match arm
                        (a.unwrap().to_bool(ctx.epsilon) | b.unwrap().to_bool(ctx.epsilon)).into()
                    }
                    (_, Some(_), Some(_)) => Value::List("()".to_string()),
                    (_, _, _) => {
                        return Err(CalculatorError::from_type(NotEnoughElementsOnTheDataStack));
                    }
                };

                ctx.stack.push(result);
                Ok(())
            }
            '~' => {
                // Negation
                let negated_value = match ctx.stack.pop() {
                    Some(Value::Single(v)) => Value::Single(v.neg()),
                    None => {
                        return Err(CalculatorError::from_type(NotEnoughElementsOnTheDataStack));
                    }
                    _ => Value::List("()".to_string()),
                };
                ctx.stack.push(negated_value);
                Ok(())
            }
            '%' => {
                // Rounding
                let rounded_value = match ctx.stack.pop() {
                    Some(Value::Single(v)) => Value::Single(v.round()),
                    Some(Value::List(_)) => Value::List("()".to_string()),
                    None => {
                        return Err(CalculatorError::from_type(NotEnoughElementsOnTheDataStack));
                    }
                };
                ctx.stack.push(rounded_value);
                Ok(())
            }
            '_' => {
                // Square root
                match ctx.stack.pop() {
                    Some(Value::Single(v)) => {
                        if v.is_sign_positive() {
                            ctx.stack.push(Value::Single(v.sqrt()));
                        }
                        Ok(())
                    }
                    None => Err(CalculatorError::from_type(NotEnoughElementsOnTheDataStack)),
                    _ => Ok(()),
                }
            }
            '!' => {
                // Copy
                if let Some(value) = ctx.stack.pop() {
                    if let Value::Single(v) = value {
                        let n = v.round() as usize;
                        // Why the +1? Because the topmost value has to be replaced.
                        if ctx.stack.len() + 1 >= n {
                            let nth_elem =
                                ctx.stack.get(ctx.stack.len() - (n - 1)).expect("").clone();
                            ctx.stack.push(nth_elem);
                        } else {
                            return Err(CalculatorError::from_type(
                                NotEnoughElementsOnTheDataStack,
                            ));
                        }
                    } else {
                        // Put it back onto the stack
                        ctx.stack.push(value);
                    }
                    Ok(())
                } else {
                    Err(CalculatorError::from_type(EmptyDataStack))
                }
            }
            '$' => {
                // Delete
                if let Some(value) = ctx.stack.pop() {
                    if let Value::Single(v) = value {
                        let n = v.round() as usize;
                        if ctx.stack.len() >= n {
                            ctx.stack.remove(ctx.stack.len() - n);
                        } else {
                            println!("Delete: Stack is too short");
                            return Err(CalculatorError::from_type(
                                NotEnoughElementsOnTheDataStack,
                            ));
                        }
                    } else {
                        // Put it back onto the stack
                        ctx.stack.push(value);
                    }
                    Ok(())
                } else {
                    Err(CalculatorError::from_type(EmptyDataStack))
                }
            }
            '@' => {
                // Apply immediately
                if let Some(top_stack_element) = ctx.stack.pop() {
                    match top_stack_element {
                        Value::Single(number) => {
                            // Put back, should not have been popped out of the stack.
                            ctx.stack.push(Value::Single(number));
                        }
                        Value::List(cmd) => {
                            cmd.chars()
                                .rev()
                                .for_each(|c| ctx.command_stream.push_front(c));
                        }
                    }
                    Ok(())
                } else {
                    Err(CalculatorError::from_type(EmptyDataStack))
                }
            }
            '\\' => {
                // Apply later
                if let Some(top_stack_element) = ctx.stack.pop() {
                    match top_stack_element {
                        Value::Single(number) => {
                            // Put back, should not have been popped out of the stack.
                            ctx.stack.push(Value::Single(number));
                        }
                        Value::List(cmd) => {
                            cmd.chars().for_each(|c| ctx.command_stream.push_back(c));
                        }
                    }
                    Ok(())
                } else {
                    Err(CalculatorError::from_type(EmptyDataStack))
                }
            }
            '#' => {
                // Stack size
                let length = ctx.stack.len();
                ctx.stack.push(Value::Single(length as f64));
                Ok(())
            }
            '\'' => {
                // Read input
                let mut buffer = String::new();
                if let Err(e) = ctx.input.read_line(&mut buffer) {
                    return Err(CalculatorError::from_type(IOError(Box::from(
                        e.to_string(),
                    ))));
                }
                buffer = buffer.trim_end().to_string();

                if let Ok(float_value) = buffer.trim_end().parse::<f64>() {
                    ctx.stack.push(Value::Single(float_value));
                } else if buffer.matches('(').count() == buffer.matches(')').count() {
                    ctx.stack.push(Value::List(buffer));
                } else {
                    ctx.stack.push(Value::List("()".to_string()));
                }
                Ok(())
            }
            '"' => {
                // Write output
                if let Some(value) = ctx.stack.pop() {
                    match write!(&mut ctx.output, "{}", value) {
                        Ok(_) => Ok(()),
                        Err(e) => Err(CalculatorError::from_type(IOError(Box::from(
                            e.to_string(),
                        )))),
                    }
                } else {
                    Err(CalculatorError::from_type(EmptyDataStack))
                }
            }
            _c => Ok(()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::TestCalculator;

    #[test]
    fn create_whole_number() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        assert_eq!(mode.read(&mut calc, '5'), Ok(()));
        assert_eq!(calc.operation_mode, -1);
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(5.0));
    }

    #[test]
    fn create_decimal_place() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        assert_eq!(mode.read(&mut calc, '.'), Ok(()));
        assert_eq!(calc.operation_mode, -2);
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(0.0));
    }

    #[test]
    fn create_list() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        assert_eq!(mode.read(&mut calc, '('), Ok(()));
        assert_eq!(calc.operation_mode, 1);
        assert_eq!(calc.stack.pop().unwrap(), Value::List("".to_string()))
    }

    #[test]
    fn register_store_load() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        // Storing ASCII values of chars a..z to their registers
        for i in 0..26 {
            let c = char::from(i + b'a');
            calc.stack.push(Value::Single(i as f64));
            assert_eq!(mode.read(&mut calc, c.to_ascii_uppercase()), Ok(()));
        }

        // Loading those values and checking them
        for i in 0..26 {
            let c = char::from(i + b'a');
            assert_eq!(mode.read(&mut calc, c), Ok(()));
            assert_eq!(calc.stack.pop().unwrap(), Value::Single(i as f64));
        }
    }

    #[test]
    fn comparison_operations() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        let test_cases = vec![
            // Equal
            (2.0, 3.0, '=', 0.0),
            (2.0, 2.0, '=', 1.0),
            // Lower
            (2.0, 3.0, '<', 1.0),
            (3.0, 2.0, '<', 0.0),
            // Greater
            (2.0, 3.0, '>', 0.0),
            (3.0, 2.0, '>', 1.0),
            (1.0, 0.0, '>', 1.0),
        ];

        for case in test_cases {
            calc.stack.push(Value::Single(case.0));
            calc.stack.push(Value::Single(case.1));
            assert_eq!(mode.read(&mut calc, case.2), Ok(()));
            assert_eq!(calc.stack.last().unwrap(), &Value::Single(case.3));
            let _ = calc.stack.pop();
        }
    }

    #[test]
    fn check() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        calc.stack.push(Value::Single(1.0));
        assert_eq!(mode.read(&mut calc, '?'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(0.0));

        calc.stack.push(Value::Single(0.0));
        assert_eq!(mode.read(&mut calc, '?'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(1.0));

        calc.stack.push(Value::List("()".to_string()));
        assert_eq!(mode.read(&mut calc, '?'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(1.0));

        calc.stack.push(Value::List("(abc)".to_string()));
        assert_eq!(mode.read(&mut calc, '?'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(0.0));
    }

    #[test]
    fn arithmetic_operations() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        let test_cases = vec![
            // Plus
            (2.0, 3.0, '+', 5.0),
            (2.0, -3.0, '+', -1.0),
            // Minus
            (2.0, 3.0, '-', -1.0),
            (2.0, -3.0, '-', 5.0),
            // Multiplication
            (2.0, 3.0, '*', 6.0),
            (2.0, -3.0, '*', -6.0),
            // Division
            (6.0, 3.0, '/', 2.0),
            (6.0, -3.0, '/', -2.0),
            // AND
            (1.0, 5.0, '&', 1.0),
            (0.0, -3.0, '&', 0.0),
            // OR
            (1.0, 5.0, '|', 1.0),
            (0.0, -3.0, '|', 1.0),
        ];

        for case in test_cases {
            calc.stack.push(Value::Single(case.0));
            calc.stack.push(Value::Single(case.1));
            assert_eq!(mode.read(&mut calc, case.2), Ok(()));
            assert_eq!(calc.stack.last().unwrap(), &Value::Single(case.3));
            let _ = calc.stack.pop();
        }
    }

    #[test]
    fn negate() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        // Single-Value negation
        calc.stack.push(Value::Single(42.0));
        assert_eq!(mode.read(&mut calc, '~'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(-42.0));

        // List negation
        calc.stack.push(Value::List("(abc)".to_string()));
        assert_eq!(mode.read(&mut calc, '~'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::List("()".to_string()));
    }

    #[test]
    fn round() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        calc.stack.push(Value::Single(0.5));
        assert_eq!(mode.read(&mut calc, '%'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(1.0));

        calc.stack.push(Value::Single(0.49));
        assert_eq!(mode.read(&mut calc, '%'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(0.0));

        calc.stack.push(Value::Single(-0.5));
        assert_eq!(mode.read(&mut calc, '%'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(-1.0));

        calc.stack.push(Value::Single(0.49));
        assert_eq!(mode.read(&mut calc, '%'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(0.0));

        calc.stack.push(Value::List("(abc)".to_string()));
        assert_eq!(mode.read(&mut calc, '%'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::List("()".to_string()));
    }

    #[test]
    fn square_root() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        calc.stack.push(Value::Single(16.0));
        assert_eq!(mode.read(&mut calc, '_'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(4.0));

        calc.stack.push(Value::Single(4.0));
        assert_eq!(mode.read(&mut calc, '_'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(2.0));
    }

    #[test]
    fn stack_size() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        assert_eq!(mode.read(&mut calc, '#'), Ok(()));
        assert_eq!(mode.read(&mut calc, '#'), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(1.0));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(0.0));
    }

    #[test]
    fn read_input_number() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        calc.input = b"-42.1337";

        assert_eq!(mode.read(&mut calc, '\''), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::Single(-42.1337));
    }

    #[test]
    fn read_input_string() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        calc.input = b"(Hello)";

        assert_eq!(mode.read(&mut calc, '\''), Ok(()));
        assert_eq!(
            calc.stack.pop().unwrap(),
            Value::List("(Hello)".to_string())
        );
    }

    #[test]
    fn read_input_not_well_formed() {
        let mut calc = TestCalculator::default();
        let mode = ExecutionMode::default();

        calc.input = b"(abc";

        assert_eq!(mode.read(&mut calc, '\''), Ok(()));
        assert_eq!(calc.stack.pop().unwrap(), Value::List("()".to_string()));
    }
}
