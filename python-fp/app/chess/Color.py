from dataclasses import dataclass
from enum import Enum


class Color(Enum):
    WHITE = 1
    BLACK = 2


def other(col: Color) -> Color:
    if col == Color.BLACK:
        return Color.WHITE
    return Color.BLACK