from typing import Optional, Tuple
import re

from app.chess.utils import Maybe, Nothing, Just

Position = Tuple[int, int]


def toIndex(pos: Position) -> int:
    return 8 * pos[0] + pos[1]


def valid(pos: Position) -> bool:
    inRange = lambda x: x in range(0, 8)
    (f, r) = pos
    return inRange(f) and inRange(r)


def charRangeInclusive(a, b):
    return list(map(lambda x: chr(x), range(ord(a), ord(b)+1)))


def fromString(posStr: str) -> Maybe[Position]:
    match = re.match("([abcdefgh])(\d)", posStr)
    if not match:
        return Nothing
    (f, r) = (match[0][0], match[0][1])
    isValid = (f in charRangeInclusive('a', 'h')) and int(r) in range(1, 9)
    if isValid:
        return Just((int(r) - 1, ord(f) - 97))
    return Nothing


def showPosition(pos: Maybe[Position]) -> str:
    def getVal(p: Position):
        (f, r) = p
        return f"{charRangeInclusive('a', 'h')[r]}{str(r+1)}"
    return pos.map(getVal).orElse("")


# print(showPosition((1, 1)))