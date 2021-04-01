from enum import Enum
from typing import List, Tuple, Callable, Iterable
from dataclasses import dataclass
from toolz import compose, curry

from app.chess.Color import Color, other


class PieceType(Enum):
    Pawn = 1
    Knight = 2
    Bishop = 3
    Rook = 4
    Queen = 5
    King = 6


@dataclass
class Piece:
    color: Color
    type: PieceType

    def __iter__(self):
        return [self.color, self.type].__iter__()

    def __str__(self):
        if self.color == Color.WHITE:
            return {PieceType.Pawn: "♙",
                    PieceType.Knight: "♘",
                    PieceType.Bishop: "♗",
                    PieceType.Rook: "♖",
                    PieceType.Queen: "♕",
                    PieceType.King: "♔"
                    }[self.type]
        else:
            return {PieceType.Pawn: "♟",
                    PieceType.Knight: "♞",
                    PieceType.Bishop: "♝",
                    PieceType.Rook: "♜",
                    PieceType.Queen: "♛",
                    PieceType.King: "♚"
                    }[self.type]

