use std::io::{BufRead, Write};

use crate::operation_modes::error::CalculatorError;
use crate::operation_modes::error::CalculatorErrorType::{
    EmptyDataStack, UnexpectedDataTypeOnDataStack, WrongOperationMode,
};
use crate::operation_modes::{CalculatorResult, OperationMode};
use crate::value::Value;
use crate::Calculator;

/// This mode is responsible for the creation of whole numbers on the data stack.
#[derive(Default)]
pub(crate) struct WholeNumberConstructionMode {}

impl<R: BufRead, W: Write> OperationMode<R, W> for WholeNumberConstructionMode {
    fn read(&self, ctx: &mut Calculator<R, W>, input: char) -> CalculatorResult<()> {
        if ctx.operation_mode != -1 {
            return Err(CalculatorError::from_type(WrongOperationMode(
                ctx.operation_mode,
            )));
        }

        match input {
            '0'..='9' => {
                // multiplies the top entry on the data stack by 10 and then adds the value of the input
                // character

                // Pop value from stack and check if Value is of type Single
                if let Some(top_stack_value) = ctx.stack.pop() {
                    if let Value::Single(mut top_stack_element) = top_stack_value {
                        // Parse input char to digit
                        let input_value = input.to_digit(10).unwrap();
                        // Add the value to the top element of the stack
                        top_stack_element = (top_stack_element * 10.0) + input_value as f64;
                        // Finally push this value back to the stack
                        ctx.stack.push(Value::Single(top_stack_element));
                        Ok(())
                    } else {
                        Err(CalculatorError::from_type(UnexpectedDataTypeOnDataStack))
                    }
                } else {
                    Err(CalculatorError::from_type(EmptyDataStack))
                }
            }
            '.' => {
                // Causes the Operation mode to become -2
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
mod test {}
