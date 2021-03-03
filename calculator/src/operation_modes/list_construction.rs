use std::io::{BufRead, Write};

use crate::operation_modes::error::CalculatorError;
use crate::operation_modes::error::CalculatorErrorType::{
    EmptyDataStack, UnexpectedDataTypeOnDataStack, WrongOperationMode,
};
use crate::operation_modes::{CalculatorResult, OperationMode};
use crate::value::Value;
use crate::Calculator;

/// This mode is responsible for the creation of lists on the data stack.
#[derive(Default)]
pub(crate) struct ListConstructionMode {}

impl<R: BufRead, W: Write> OperationMode<R, W> for ListConstructionMode {
    fn read(&self, ctx: &mut Calculator<R, W>, input: char) -> CalculatorResult<()> {
        if ctx.operation_mode <= 0 {
            return Err(CalculatorError::from_type(WrongOperationMode(
                ctx.operation_mode,
            )));
        }

        match input {
            '(' => {
                // adds '(' to the top list of the data stack and the operation mode becomes m+1
                if let Some(Value::List(mut top_stack_element)) = ctx.stack.pop() {
                    top_stack_element.push('(');
                    // increments the mode
                    ctx.operation_mode += 1;
                    // pushes it back on top of the stack
                    ctx.stack.push(Value::List(top_stack_element));
                    Ok(())
                } else {
                    Err(CalculatorError::from_type(UnexpectedDataTypeOnDataStack))
                }
            }
            ')' => {
                // adds ')' to the top list of the data stack and the operation mode becomes m-1 in
                // every case (if the operation mode becomes 0 nothing is added)
                ctx.operation_mode -= 1;
                if ctx.operation_mode > 0 {
                    return match ctx.stack.pop() {
                        Some(Value::List(mut top_stack_element)) => {
                            top_stack_element.push(')');
                            // pushes it back on top of the stack
                            ctx.stack.push(Value::List(top_stack_element));
                            Ok(())
                        }
                        Some(Value::Single(_)) => {
                            Err(CalculatorError::from_type(UnexpectedDataTypeOnDataStack))
                        }
                        None => Err(CalculatorError::from_type(EmptyDataStack)),
                    };
                }
                Ok(())
            }
            c => {
                // adds the input character to the top list of the data stack
                if let Some(top_stack_element) = ctx.stack.pop() {
                    if let Value::List(mut list) = top_stack_element {
                        list.push(c);
                        ctx.stack.push(Value::List(list));
                        Ok(())
                    } else {
                        Err(CalculatorError::from_type(UnexpectedDataTypeOnDataStack))
                    }
                } else {
                    Err(CalculatorError::from_type(EmptyDataStack))
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::TestCalculator;

    #[test]
    fn add_list() {
        let mode = ListConstructionMode::default();
        let mut calc = TestCalculator::default();

        // The calculator has to be prepared, for list construction mode. Otherwise some asserts
        // will stop this test case from execution.
        calc.operation_mode = 1;
        calc.stack.push(Value::List("".to_string()));

        assert_eq!(mode.read(&mut calc, '('), Ok(()));
        assert_eq!(calc.operation_mode, 2);
        assert_eq!(mode.read(&mut calc, 'a'), Ok(()));
        assert_eq!(calc.operation_mode, 2);
        assert_eq!(mode.read(&mut calc, ')'), Ok(()));
        assert_eq!(calc.operation_mode, 1);
        assert_eq!(mode.read(&mut calc, ')'), Ok(()));
        assert_eq!(calc.operation_mode, 0);

        assert_eq!(calc.stack.pop().unwrap(), Value::List("(a)".to_string()));
    }

    #[test]
    fn wrong_operation_mode() {
        let mode = ListConstructionMode::default();
        let mut calc = TestCalculator::default();

        calc.operation_mode = -1;

        assert_eq!(
            mode.read(&mut calc, '('),
            Err(CalculatorError::from_type(WrongOperationMode(-1)))
        );
    }

    #[test]
    fn start_sub_list_on_single_element() {
        let mode = ListConstructionMode::default();
        let mut calc = TestCalculator::default();

        calc.operation_mode = 1;
        calc.stack.push(Value::Single(0.0));

        assert_eq!(
            mode.read(&mut calc, '('),
            Err(CalculatorError::from_type(UnexpectedDataTypeOnDataStack))
        );
    }

    #[test]
    fn end_sub_list_on_single_element() {
        let mode = ListConstructionMode::default();
        let mut calc = TestCalculator::default();

        calc.operation_mode = 2;
        calc.stack.push(Value::Single(0.0));

        assert_eq!(
            mode.read(&mut calc, ')'),
            Err(CalculatorError::from_type(UnexpectedDataTypeOnDataStack))
        );
    }

    #[test]
    fn end_sub_list_on_empty_data_stack() {
        let mode = ListConstructionMode::default();
        let mut calc = TestCalculator::default();

        calc.operation_mode = 2;

        assert_eq!(
            mode.read(&mut calc, ')'),
            Err(CalculatorError::from_type(EmptyDataStack))
        );
    }
}
