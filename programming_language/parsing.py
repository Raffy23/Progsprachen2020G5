import logging
from types import prepare_class
from lexer import Token, TokenType
from parser_structures import *

logger = logging.getLogger(__name__)


def parse(tokens: List[Token]) -> Entity:
    logger.debug("Tokens: %s", tokens)
    # Generating basic functions
    context: Dict[str, Entity] = {}
    basic_functions = {}
    for name in BasicFunction.basic_operations.keys():
        basic_functions[name] = BasicFunction(name)
    context = basic_functions

    # i is the index of the current position in the token list
    # i has to have the value of the first token to be read for a new function at all times
    i = 0

    def parse_next() -> Entity:
        nonlocal i, tokens
        last_value = Entity()
        while i < len(tokens):
            current_token = tokens[i]
            type = current_token.token_type
            if type == TokenType.BRACE_OPEN:
                i += 1
                last_value = parse_record()
            elif type == TokenType.PARANTHESE_OPEN:
                i += 1
                last_value = parse_next()
            elif type == TokenType.NAME and tokens[i+1].token_type == TokenType.FUNCTION_ARROW:
                # function definition found
                last_value = parse_function()
                last_value.context = context
            elif type == TokenType.NAME:
                try:
                    last_value.add_parameter( # type: ignore
                        Name(current_token.token_string))
                    i += 1
                except AttributeError:
                    # statements have to be returned immediately, I hope
                    newvalue = parse_statement()
                    try:
                        newvalue.context.update(
                            deepcopy(last_value.pairs))  #type: ignore
                    except AttributeError:
                        pass
                    return newvalue
            elif type in [TokenType.PARANTHESE_CLOSE, TokenType.COMMA]:
                i += 1
                return last_value
            elif type == TokenType.INTEGER:
                try:
                    last_value.add_parameter( # type: ignore
                        Integer(int(current_token.token_string)))  
                    i += 1
                except AttributeError:
                    i += 1
                    return Integer(int(current_token.token_string))
            elif type == TokenType.BRACE_CLOSE:
                return last_value
            else:
                raise InvalidOperationException(
                    f'[PARSING] The token {current_token} is not expected at position {i}')

        return last_value

    def parse_function(function=None) -> Function:
        nonlocal i, tokens
        if function is None:
            function = Function()
        while i < len(tokens):
            current_token = tokens[i]
            type = current_token.token_type
            if type == TokenType.NAME and tokens[i+1].token_type == TokenType.FUNCTION_ARROW:
                function.parameters[current_token.token_string] = None
                i += 2  # skip current name + function arrow
            elif type == TokenType.PARANTHESE_OPEN:
                i += 1
                parse_function(function)
            elif type == TokenType.NAME:
                function.expression = parse_statement()  # type: ignore
                return function
            elif type in [TokenType.INTEGER, TokenType.COMMA]:
                return function
            elif type in [TokenType.PARANTHESE_CLOSE]:
                i += 1
                return function
            else:
                raise InvalidOperationException(
                    f'[FUNCTION] The token {current_token} is not expected at position {i}')

        return function

    def parse_statement() -> Entity:
        nonlocal i, tokens
        if (tokens[i].token_type != TokenType.NAME):
            raise InvalidOperationException(f'Expected a name at position {i}')
        expression = Expression(tokens[i].token_string, [])
        i += 1
        while i < len(tokens):
            token_type = tokens[i].token_type
            if token_type == TokenType.PARANTHESE_OPEN:
                i += 1
                expression.arguments.append(parse_next())
            elif token_type == TokenType.NAME:
                expression.arguments.append(Name(tokens[i].token_string))
                i += 1
            elif token_type == TokenType.INTEGER:
                expression.arguments.append(
                    Integer(int(tokens[i].token_string)))
                i += 1
            elif token_type == TokenType.PARANTHESE_CLOSE:
                i += 1
                return expression
            elif token_type in [TokenType.BRACE_CLOSE, TokenType.COMMA]:
                return expression
            elif token_type == TokenType.BRACE_OPEN:
                i += 1
                expression.arguments.append(parse_record())
            else:
                raise InvalidOperationException(
                    f'[STATEMENT] The token {tokens[i]} is not expected at position {i}')

        return expression

    def parse_record() -> Record:
        nonlocal i, tokens
        record = Record()
        while i < len(tokens):
            token_type = tokens[i].token_type
            if token_type == TokenType.NAME and tokens[i+1].token_type == TokenType.EQUAL:
                name = tokens[i].token_string
                i += 2  # jumping to first token after =
                record.pairs[name] = parse_next()
                record.pairs[name].context.update(record.pairs)
            elif token_type == TokenType.BRACE_CLOSE:
                i += 1
                return record
            elif token_type == TokenType.COMMA:
                i += 1
            else:
                raise InvalidOperationException(
                    f'[RECORD] The token {tokens[i]} is not expected at position {i}')
        return record

    return parse_next().eval(context)
