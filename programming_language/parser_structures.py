import logging
from parser_exceptions import *
from typing import Any, List, Callable, Dict, Union, TypeVar
from copy import deepcopy, copy

logger = logging.getLogger(__name__)


class Entity:
    """Entity, a supertype for all citizens of this language, who support basic operations

    Entities have to support two basic operations, truthy which determines the truth value of the entity, and eval, which evaluates the entity to a integer. Partial functions cannot be evaluated.
    """

    def __init__(self):
        super().__init__()
        self.context: Context = {}

    def truthy(self, context: Dict[str, "Entity"]) -> bool:
        """Determines the truth value of this Entity.

        In general, null like values should be false, and all other values should be true
        """
        raise NotImplementedException(
            "Cannot evaluate truthness on abstract Entity")
        return False

    def eval(self, context: Dict[str, "Entity"]) -> "Entity":
        """Evaluates this entity within a context.

        """
        raise NotImplementedException(
            "Entity superclasses cannot be evaluated")

    def __repr__(self):
        return "Entity()"


Context = Dict[str, Entity]


class Expression(Entity):
    """Expression Entity, used to describe expressions.

    Expressions do not define a new environment, they just get evaluated in an already existing environment. Therefore it needs an existing environment (as dict) to evaluate. It replaces the arguments with the value of the context, and then evaluates to another Entity.
    """

    bound_context = None

    # operation str may be changed to direct reference to a copy of this function for environment purposes
    def __init__(self, operation: str = None, arguments: List[Entity] = None):
        super().__init__()
        if (operation == None):
            operation = ""
        if (arguments == None):
            arguments = []
        self.operation = operation
        self.arguments = arguments

    """Adds a parameter to this expression

    This adds parameters which will then be then evaluated in context of the operation of this expression.
    """
    def add_parameter(self, parameter: Entity):
        self.arguments.append(parameter)

    def eval(self, context: Dict[str, Entity]) -> Entity:
        logger.debug("Evaluating operation %s", self.operation)

        current_context = copy(context)
        if self.bound_context is not None:
            current_context.update(self.bound_context)
        current_context.update(self.context)

        function = deepcopy(current_context[self.operation])

        # make difference in evaluation between conditional statements and others
        if self.operation != "cond":
            for argument in self.arguments:
                try:

                    evaluated_arg = None
                    if type(argument) is Name:
                        evaluated_arg = argument.bind( # type: ignore
                            current_context)  # type: ignore
                    else:
                        evaluated_arg = argument.eval(current_context)

                    if type(function) is Record:
                        try:
                            return function.pairs[evaluated_arg.name].eval(current_context) # type: ignore
                        except AttributeError:
                            raise InvalidArgumentException(
                                "A record can only be evaluated as function when given a name")

                    function.add_parameter(evaluated_arg)  # type: ignore
                except KeyError:
                    raise InvalidArgumentException(
                        f'Argument {argument} is not defined in the context.')
                except AttributeError:
                    raise InvalidOperationException(
                        f'Operation {self.operation} is no valid function.')

            value = function.eval(current_context)
            return value
        else:
            try:
                function.add_parameter(self.arguments[0].eval( # type: ignore
                    current_context))
                function.add_parameter(self.arguments[1])  # type: ignore
                function.add_parameter(self.arguments[2])  # type: ignore
            except AttributeError:
                raise InvalidOperationException(
                    f'Operation {self.operation} is no valid function.')
            return function.eval(current_context)

    def __repr__(self) -> str:
        return f'Expression(operation: {self.operation}, arguments: {self.arguments})'

    def __str__(self) -> str:
        return f'{self.operation} {" ".join(map(lambda x: x.__str__(), self.arguments))}'


class Name(Expression):
    """Name Entity, used to describe names.

    Names evaluate to its Entity value given in the context
    """

    bound_context: Union[Context, None] = None

    def __init__(self, name: str):
        super().__init__()
        self.name = name

    def truthy(self, context: Context):
        a = context[self.name].eval(
            copy(context)).truthy(copy(context))
        return a

    """Returns a new Name variable with the specified bounded context.

    Returns a new Name, which has a reference to the bounded context it originated, to provide proper name resolution.
    """
    def bind(self, context: Context) -> Entity:
        new_name = Name(self.name)
        new_name.bound_context = context
        return new_name

    def eval(self, context: Context) -> Entity:

        if self.bound_context is None:
            return context[self.name].eval(context)
        else:
            return self.bound_context[self.name].eval(self.bound_context)

    def __repr__(self) -> str:
        return f'Name(name: {self.name})'

    def __str__(self) -> str:
        return self.name


class Integer(Expression):
    """Integer Entity, used to describe integers.

    Integers are evaluated to False if they are 0, and True otherwise.
    """
    value: int

    def __init__(self, value: int):
        super().__init__()
        self.value = value

    def truthy(self, context: Context) -> bool:
        return self.value != 0

    def eval(self, context: Context) -> Entity:
        return self

    def __repr__(self):
        return f'Integer(value: {self.value})'

    def __str__(self) -> str:
        return str(self.value)


class Function(Entity):
    """Function Entity, used to describe functions.

    Functions are per default evaluated to True.
    They are very similar to Records, basically providing an environment for the expression which makes out the body of the function, which when evaluated returns the result of the function.
    """
    parameters: Dict[str, Union[Entity, None]]

    expression: Union[Expression, None]

    def __init__(self, parameters=None, expression: Expression = None):
        super().__init__()
        if (parameters is None):
            parameters = {}
        self.parameters = parameters
        self.expression = expression

    def number_of_parameters(self):
        return len(self.parameters)

    def add_parameter(self, parameter: Entity):
        """Sets parameter value for a parameter of this function

        Fills parameters per position.
        Example: Function with two parameters, x and y.
        First call to this function sets parameter x, second call sets parameter y. Used for partial functions.
        """
        for key, value in self.parameters.items():
            if (value is None):
                self.parameters[key] = parameter
                return

    def eval(self, context: Context) -> Entity:
        if (self.expression is None):
            raise InvalidOperationException("Expression for function not set")
        for x in self.parameters.items():
            if (x[1] is None):
                return self
        newcontext = copy(context)
        newcontext.update(self.context)
        newcontext.update(self.parameters)  # type: ignore
        a = self.expression.eval(newcontext)
        return a

    def truthy(self, context: Context) -> bool:
        try:
            a = self.eval(context).truthy(context)
            return a
        except (NotEnoughArgumentsException, InvalidArgumentException):
            logger.warning("CAN NOT EVAL EXPRESSION IN BOOLEAN POSITION!")
            return False

    def __repr__(self) -> str:
        return f'Function(parameters: {self.parameters}, expression: {self.expression.__repr__()})'

    def __str__(self) -> str:
        return f'{" -> ".join(map(lambda item: f"{item[0]} ({item[1]})", self.parameters.items()))} -> {self.expression.__str__()})'

    def __deepcopy__(self, memodict):
        return Function(deepcopy(self.parameters), deepcopy(self.expression))


class BasicFunction(Function):
    """Basic function entity, evaluates Basic Functions like add or sub

    Contains logic to evaluate basic functions within our Entity class scheme
    """
    basic_operations: Dict[str, Callable[..., Any]] = {
        "add": lambda x, y: x + y,
        "sub": lambda x, y: x - y,
        "mult": lambda x, y: x * y,
        "div": lambda x, y: x / y,
        "cond": lambda x, y, z: y if x else z
    }

    parameters: List[Entity]

    def __init__(self, function_name: str):
        super().__init__()
        self.function_name = function_name
        self.parameters = []

    def number_of_parameters(self):
        return 3 if self.function_name == "cond" else 2

    def add_parameter(self, parameter: Entity):
        self.parameters.append(parameter)

    def truthy(self, context: Context) -> bool:
        return self.eval(context).truthy(context)

    def eval(self, context: Context) -> Entity:
        logger.debug("eval " + self.function_name)

        # Check argument length matches parameter count
        if len(self.parameters) > self.number_of_parameters():
            raise TooManyArgumentsException(
                f'Too many arguments for this function. Expected: {self.number_of_parameters()} Got: {len(self.parameters)}')
        if len(self.parameters) < self.number_of_parameters():
            return self

        if (self.function_name != "cond"):
            # evaluate all parameters
            try:
                evaluated_params = map(lambda x: x.eval(
                    context).value, self.parameters)  # type: ignore
            except AttributeError:
                raise InvalidArgumentException(
                    "Need integer argument for basic arithmetic functions")
            # evaluate basic function
            r = Integer(BasicFunction.basic_operations[self.function_name](
                *evaluated_params))
            return r
        else:
            # evaluate basic function

            if self.parameters[0].truthy(context):
                a = self.parameters[1].eval(context)
                return a

            else:
                a = self.parameters[2].eval(context)
                return a

    def __repr__(self) -> str:
        return f'BasicFunction(function_name: {self.function_name}, Parameters: {self.parameters})'

    def __str__(self) -> str:
        return f'{self.function_name} {" ".join(map(lambda x: x.__str__(), self.parameters))})'

    def __deepcopy__(self, memodict):
        return BasicFunction(self.function_name)


class Record(Entity):
    """ Record Entity, used to describe records (as first class citizens of this language it is an entity as anything else).
    Records are environments consisting of key - value pairs, with string as keys and other entities.

    Records are evaluated as False in boolean comparisons if they are empty, and True otherwise.
    """
    pairs: Dict[str, Entity]

    def __init__(self):
        super().__init__()
        self.pairs = {}

    def truthy(self, context: Context) -> bool:
        return bool(self.pairs)

    def eval(self, context: Context) -> Entity:

        if len(self.pairs) == 0:
            return self

        value = Record()
        for item in self.pairs.items():
            a = item[1].eval(context)
            value.pairs[item[0]] = a

        return value

    def __repr__(self):
        return f'Record(pairs: {self.pairs})'

    def __str__(self):
        return f'{{{", ".join(map(lambda x: f"{x[0]}={x[1]}", self.pairs.items()))}}}'


S = TypeVar("S")
T = TypeVar("T")


def flat_dict(dict_list: List[Dict[S, T]]) -> Dict[S, T]:
    """ Maps a list of dicts to a single dict containing all its values. 

    Values later in the list (i.E. with higher index values) will overwrite Values defined by earlier dicts with the same key.
    """
    result = {}
    for dict in dict_list:
        result.update(dict)
    return result
