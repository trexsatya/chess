from dataclasses import dataclass
from typing import Optional

from toolz import compose

from app.chess.ChessBoard import PieceType
from app.chess.Position import Position, fromString
from app.chess.utils import Maybe, Nothing, Just, Either, Right, Left, mapl, when


class GameCommands:
    pass


@dataclass
class Pick(GameCommands):
    position: Position


@dataclass
class Move(GameCommands):
    fromPos: Position
    toPos: Position


@dataclass
class ShowBoard(GameCommands):
    pass


@dataclass
class Promotion(GameCommands):
    position: Position
    pieceType: PieceType


def trim(s: str):
    return s.strip()


def parsePromoteCommand(args) -> Either[str, GameCommands]:
    if len(args) != 2:
        return Left("If you are trying to promote a pawn, type promote <position> <piece name>")

    pos = fromString(args[0])

    def piece():
        return {
                "rook": Just(PieceType.Rook),
                "queen": Just(PieceType.Queen),
                "bishop": Just(PieceType.Bishop),
                "knight": Just(PieceType.Knight)
            }.get(args[1], Nothing)

    return pos.map(lambda p: piece().map(lambda x: Right(Promotion(p, x)))
                                    .orElse(Left("Invalid piece. You can promote to queen, rook, bishop, or knight"))
                   ).orElse(Left("Invalid position. First argument to promote must be a position e.g. a1"))


def parsePickCommand(pos: str):
    return fromString(pos).map(lambda x: Right(Pick(x)))\
                    .orElse(Left("invalid position. If you want to highlight the moves from a position, "
                                 "give just a position e.g. a1"))


def parseMoveCommand(fromPosStr: str, toPosStr: str):
    msg = "invalid position. If you are trying to move a pice give two positions separaated by space e.g a1 a2"
    return fromString(fromPosStr).map(lambda x: fromString(toPosStr).map(lambda y: Right(Move(x, y)))
                                                                    .orElse(Left(msg))
                                      ).orElse(Left(msg))


def parseInput(inputStr: Maybe[str]) -> Either[str, GameCommands]:
    # if inputStr.map(compose(len, trim)).orElse(0) == 0:
    #     return

    def handleCmd(args):
        if args[0] == "promote":
            return parsePromoteCommand(args[1:])
        if len(args) == 1:
            return parsePickCommand(args[0])
        if len(args) == 2:
            return parseMoveCommand(args[0], args[1])
        return Left("Unrecognised command")

    return inputStr.map(trim).filter(lambda x: x)\
                             .map(lambda x: x.split(" "))\
                             .map(lambda lst: mapl(trim, lst))\
                             .map(handleCmd)\
                             .orElse(Right(ShowBoard()))


print(parseInput(Just(" a1 a3")))
