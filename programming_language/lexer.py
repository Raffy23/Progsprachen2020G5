from enum import Enum, unique, auto
from typing import List


class TokenType(Enum):
    """Enum of possible types for the token class

    """
    INTEGER = auto()
    NAME = auto()
    EQUAL = auto()
    PARANTHESE_OPEN = auto()
    PARANTHESE_CLOSE = auto()
    BRACE_OPEN = auto()
    BRACE_CLOSE = auto()
    FUNCTION_ARROW = auto()
    COMMA = auto()
    MINUS = auto()


class Token:
    """Language token to distinguish language elements

    This class represents a single token in this programming language, like integers or certain symbols.
    """
    token_type: TokenType
    token_string: str

    def __init__(self, token_type, token_string):
        self.token_type = token_type
        self.token_string = token_string

    def appendToToken(self, char):
        """Appends a character to this Token

        Adds a new character to the existing Token
        """
        self.token_string += char

    def __str__(self):
        return f'Token({self.token_type}, "{self.token_string}")'

    __repr__ = __str__


class InvalidSyntaxException(Exception):
    """Exception raised for errors in the syntax of the provided code.

    Attributes:
        expression -- input expression in which the error occurred
        message -- explanation of the error
    """

    def __init__(self, expression, message):
        self.expression = expression
        self.message = message


def tokenize(input: str) -> List[Token]:
    """Function to tokenize a given string with instructions

    Takes a string with valid instructions, and outputs a sequence of tokens of type "Token".
    Can raise an InvalidSyntaxException
    """
    result_tokens = []
    last_token = None
    for char in input:
        if char.isdecimal():
            if last_token == None:
                last_token = Token(TokenType.INTEGER, char)
            elif last_token.token_type in [TokenType.NAME, TokenType.INTEGER]:
                last_token.appendToToken(char)
            elif last_token.token_type == TokenType.MINUS:
                last_token.token_type = TokenType.INTEGER
                last_token.appendToToken(char)
            else:
                raise InvalidSyntaxException(
                    char, f"Invalid character {char} after {last_token.token_string}. Excpected to be of type {last_token.token_type}.")
        elif char.isalpha():
            if last_token == None:
                last_token = Token(TokenType.NAME, char)
            elif last_token.token_type == TokenType.NAME:
                last_token.appendToToken(char)
            else:
                raise InvalidSyntaxException(
                    char, f"Invalid character {char} after {last_token.token_string}. Excpected to be of type {last_token.token_type}.")
        elif char == ">":
            if last_token.token_type == TokenType.MINUS:
                last_token = None
                result_tokens.append(Token(TokenType.FUNCTION_ARROW, "->"))
        else:
            if (last_token != None):
                result_tokens.append(last_token)
                last_token = None
            if char == "(":
                result_tokens.append(Token(TokenType.PARANTHESE_OPEN, char))
            elif char == "-":
                last_token = Token(TokenType.MINUS, char)
            elif char == ")":
                result_tokens.append(Token(TokenType.PARANTHESE_CLOSE, char))
            elif char == "{":
                result_tokens.append(Token(TokenType.BRACE_OPEN, char))
            elif char == "}":
                result_tokens.append(Token(TokenType.BRACE_CLOSE, char))
            elif char == "=":
                result_tokens.append(Token(TokenType.EQUAL, char))
            elif char == ",":
                result_tokens.append(Token(TokenType.COMMA, char))
            elif not char.isspace():
                assert False
    # Make sure the last token is also added, if there is no space / newline at the end.
    if last_token != None:
        result_tokens.append(last_token)
    return result_tokens
