from dataclasses import dataclass
from typing import Optional, List

from toolz import compose

from app.chess.Pieces import PieceType
from app.chess.Position import Position, fromString
from app.chess.utils import Maybe, Nothing, Just, Either, Right, Left, mapl, when, bind, maybeToLeft


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


def parsePromoteCommand(args) -> Either[List[str], GameCommands]:
    if len(args) != 2:
        return Left(["If you are trying to promote a pawn, type promote <position> <piece name>"])

    pos = fromString(args[0])

    def piece():
        return {
            "rook": Just(PieceType.Rook),
            "queen": Just(PieceType.Queen),
            "bishop": Just(PieceType.Bishop),
            "knight": Just(PieceType.Knight)
        }.get(args[1], Nothing)

    return pos.map(lambda p: piece().map(lambda x: Right(Promotion(p, x)))
                   .orElse(Left(["Invalid piece. You can promote to queen, rook, bishop, or knight"]))
                   ).orElse(Left(["Invalid position. First argument to promote must be a position e.g. a1"]))


def parsePickCommand(pos: str):
    return fromString(pos).map(lambda x: Right(Pick(x))) \
        .orElse(Left(["invalid position. If you want to highlight the moves from a position, "
                      "give just a position e.g. a1"]))


def parseMoveCommand(fromPosStr: str, toPosStr: str):
    msg = ["invalid position. If you are trying to move a piece, give two positions separated by space e.g a1 a2"]
    # return fromString(fromPosStr).map(lambda x: fromString(toPosStr).map(lambda y: Right(Move(x, y)))
    #                                                                 .orElse(Left(msg))
    #                                   ).orElse(Left(msg))
    from_ = maybeToLeft(fromString(fromPosStr), ["Invalid source position!"])
    to_   = maybeToLeft(fromString(toPosStr), ["Invalid target position!"])

    res = from_ |bind| (lambda x:
                        to_ |bind| (lambda y:
                                    Right(Move(x, y))))
    return res


def parseInput(inputStr: Maybe[str]) -> Either[List[str], GameCommands]:
    def handleCmd(args):
        if args[0] == "promote":
            return parsePromoteCommand(args[1:])
        if len(args) == 1:
            return parsePickCommand(args[0])
        if len(args) == 2:
            return parseMoveCommand(args[0], args[1])
        return Left(["Unrecognised command"])

    return inputStr.map(trim).filter(lambda x: x) \
        .map(lambda x: x.split(" ")) \
        .map(mapl(trim)) \
        .map(handleCmd) \
        .orElse(Right(ShowBoard()))
