class TooManyArgumentsException(Exception):
    """Exception raised for too many arguments for a function

    Attributes:
        message -- explanation of the error
    """

    def __init__(self, message):
        self.message = message


class NotEnoughArgumentsException(Exception):
    """Exception raised for too few arguments for a function

    Attributes:
        message -- explanation of the error
    """

    def __init__(self, message):
        self.message = message


class NotImplementedException(Exception):
    """Exception raised for not implemented actions

    Attributes:
        message -- explanation of the error
    """

    def __init__(self, message):
        self.message = message


class InvalidArgumentException(Exception):

    def __init__(self, message: str) -> None:
        self.message = message


class InvalidOperationException(Exception):

    def __init__(self, message: str) -> None:
        self.message = message
