use std::io::{BufRead, Write};

use crate::operation_modes::error::CalculatorError;
use crate::operation_modes::error::CalculatorErrorType::{
    UnexpectedDataTypeOnDataStack, WrongOperationMode,
};
use crate::operation_modes::{CalculatorResult, OperationMode};
use crate::value::Value;
use crate::Calculator;

/// This mode is responsible for the creation of decimal places of a Value::Single.
#[derive(Default)]
pub(crate) struct DecimalPlaceConstructionMode {}

impl<R: BufRead, W: Write> OperationMode<R, W> for DecimalPlaceConstructionMode {
    fn read(&self, ctx: &mut Calculator<R, W>, input: char) -> CalculatorResult<()> {
        if ctx.operation_mode >= -1 {
            return Err(CalculatorError::from_type(WrongOperationMode(
                ctx.operation_mode,
            )));
        }

        match input {
            '0'..='9' => {
                // adds the floating point value of the input character, multiplied by 10^{m+1}
                // to the top entry of the data stack

                // Pop value from stack and check if Value is of type Single
                if let Value::Single(mut top_stack_element) = ctx
                    .stack
                    .pop()
                    .expect("Top element of the stack has to be a whole number.")
                {
                    // Parse input char to digit
                    let input_value = input.to_digit(10).unwrap();
                    // Add the value to the top element of the stack
                    let value = input_value as f64 * 10_f64.powi(ctx.operation_mode as i32 + 1);
                    top_stack_element += value;
                    // Finally push this value back to the stack
                    ctx.stack.push(Value::Single(top_stack_element));
                    // .. and decrease the operation mode
                    ctx.operation_mode -= 1;
                    Ok(())
                } else {
                    Err(CalculatorError::from_type(UnexpectedDataTypeOnDataStack))
                }
            }
            '.' => {
                // Pushes a number of value 0 onto the data stack and the operation mode becomes -2
                // this is, initiates the construction of a new number
                ctx.stack.push(Value::Single(0.0));
                ctx.operation_mode = -2;
                Ok(())
            }
            c => {
                // Operation mode becomes 0, then this character is executed using execution mode 0
                ctx.operation_mode = 0;
                // returns the result of the mode 0
                Calculator::determine_mode(0).read(ctx, c)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::TestCalculator;

    #[test]
    fn create_three_decimal_places() {
        let mode = DecimalPlaceConstructionMode::default();
        let mut calc = TestCalculator::default();

        calc.operation_mode = -2;

        assert_eq!(mode.read(&mut calc, '.'), Ok(()));
        assert_eq!(calc.operation_mode, -2);
        assert_eq!(mode.read(&mut calc, '1'), Ok(()));
        assert_eq!(calc.operation_mode, -3);
        assert_eq!(mode.read(&mut calc, '2'), Ok(()));
        assert_eq!(calc.operation_mode, -4);
        assert_eq!(mode.read(&mut calc, '3'), Ok(()));
        assert_eq!(calc.operation_mode, -5);

        let value = calc.stack.pop().unwrap();
        assert!(value.is_single());

        let a = match value {
            Value::Single(f) => f,
            Value::List(_) => unreachable!(),
        };
        let b = 0.123_f64;
        assert!((a - b).abs() < 0.00001f64);
    }

    #[test]
    fn create_two_decimal_numbers() {
        let mode = DecimalPlaceConstructionMode::default();
        let mut calc = TestCalculator::default();

        calc.operation_mode = -2;

        assert_eq!(mode.read(&mut calc, '.'), Ok(()));
        assert_eq!(calc.operation_mode, -2);
        assert_eq!(mode.read(&mut calc, '1'), Ok(()));
        assert_eq!(calc.operation_mode, -3);
        assert_eq!(mode.read(&mut calc, '2'), Ok(()));
        assert_eq!(calc.operation_mode, -4);
        assert_eq!(mode.read(&mut calc, '3'), Ok(()));
        assert_eq!(calc.operation_mode, -5);
        assert_eq!(mode.read(&mut calc, '.'), Ok(()));
        assert_eq!(calc.operation_mode, -2);
        assert_eq!(mode.read(&mut calc, '3'), Ok(()));
        assert_eq!(calc.operation_mode, -3);
        assert_eq!(mode.read(&mut calc, '2'), Ok(()));
        assert_eq!(calc.operation_mode, -4);
        assert_eq!(mode.read(&mut calc, '1'), Ok(()));
        assert_eq!(calc.operation_mode, -5);

        let value = calc.stack.pop().unwrap();
        assert!(value.is_single());

        let a = match value {
            Value::Single(f) => f,
            Value::List(_) => unreachable!(),
        };
        let b = 0.321_f64;
        assert!((a - b).abs() < 0.00001f64);

        let value = calc.stack.pop().unwrap();
        assert!(value.is_single());

        let a = match value {
            Value::Single(f) => f,
            Value::List(_) => unreachable!(),
        };
        let b = 0.123_f64;
        assert!((a - b).abs() < 0.00001f64);
    }
}
