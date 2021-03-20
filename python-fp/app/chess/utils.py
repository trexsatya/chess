import itertools
from types import FunctionType
from typing import List, TypeVar, Callable, Iterable, Generic, Tuple

from dataclasses import dataclass
from toolz import assoc_in

T = TypeVar('T')
U = TypeVar('U')


def mapl(fn: Callable, items: Iterable) -> List:
    return list(map(fn, items))


concat = lambda x: list(itertools.chain(*x))


def filterl(fn: Callable, items) -> List:
    return list(filter(fn, items))


def chunks(lst, n):
    """Yield successive n-sized chunks from lst."""
    for i in range(0, len(lst), n):
        yield lst[i:i + n]


class Monad:
    # pure :: a -> M a
    @staticmethod
    def pure(x):
        raise Exception("pure method needs to be implemented")

    # flat_map :: # M a -> (a -> M b) -> M b
    def flat_map(self, f):
        raise Exception("flat_map method needs to be implemented")

    # map :: # M a -> (a -> b) -> M b
    def map(self, f):
        return self.flat_map(lambda x: self.pure(f(x)))


def identity(val: T):
    return val


class Maybe(Monad, Generic[T]):
    # pure :: a -> Option a
    @staticmethod
    def pure(x: T):
        return Just(x)

    # flat_map :: # Maybe a -> (a -> Maybe b) -> Maybe b
    def flat_map(self, f):
        if self.defined:
            return f(self.value)
        else:
            return Nothing

    def filter(self, f):
        if self.defined and f(self.value):
            return self.pure(self.value)
        else:
            return Nothing

    def orElse(self, default: U):
        if self.defined:
            return self.value
        else:
            return default


class Just(Maybe):
    def __init__(self, value):
        self.value = value
        self.defined = True

    def __str__(self):
        return f"Just({self.value})"


class Nil(Maybe):
    def __init__(self):
        self.value = None
        self.defined = False

    def __str__(self):
        return f"Nothing"

Nothing = Nil()


def fromNullable(val: T):
    if not val:
        return Nothing
    else:
        return Just(val)


class Either(Monad, Generic[T, U]):
    # pure :: a -> Either a
    @staticmethod
    def pure(value: U):
        return Right(value)

    # flat_map :: # Either a -> (a -> Either b) -> Either b
    def flat_map(self, f):
        if self.is_left:
            return self
        else:
            return f(self.value)


@dataclass
class Left(Either, Generic[T]):
    value: T

    def __init__(self, value):
        self.value = value
        self.is_left = True

    def __str__(self):
        return f"Left({self.value})"


class Right(Either, Generic[U]):
    def __init__(self, value):
        self.value = value
        self.is_left = False

    def __str__(self):
        return f"Right({self.value})"


_ = None


def when(val: T, cases: dict, default=None):
    found = None
    for (k, v) in cases.items():
        if k and k(val):
            found = v
    if not found:
        found = cases.get("else", cases.get(_, cases.get("otherwise")))
    if not found:
        found = default
    return found() if found and isinstance(found, Callable) else found


def updatedList(items: List[T], values: List[Tuple[int, T]]) -> List[T]:
    values = dict(values)
    return [(values[x] if x in values else items[x]) for x in range(len(items))]


def lazy(val):
    return lambda x: val


# https://code.activestate.com/recipes/384122/
class Infix:
    def __init__(self, function):
        self.function = function

    def __ror__(self, other):
        return Infix(lambda x, self=self, other=other: self.function(other, x))

    def __or__(self, other):
        return self.function(other)

    def __rlshift__(self, other):
        return Infix(lambda x, self=self, other=other: self.function(other, x))

    def __rshift__(self, other):
        return self.function(other)

    def __call__(self, value1, value2):
        return self.function(value1, value2)
