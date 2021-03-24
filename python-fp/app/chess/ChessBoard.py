from dataclasses import dataclass
from enum import Enum
from typing import List, Tuple, Callable, Iterable

from toolz import compose, curry

from app.chess.Color import Color, other
from app.chess.Position import charRangeInclusive, Position, toIndex, valid
from app.chess.utils import mapl, concat, chunks, Maybe, Nothing, Just, filterl, updatedList, Either, Left, Right, \
    Infix, lazy


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


ANSI_RESET = "\u001B[0m"


class BackgroundColor(Enum):
    Yellow = 1
    Maroon = 2
    Cyan = 3
    White = 4


def ansiCode(c: BackgroundColor) -> str:
    return {
        BackgroundColor.Yellow: "\u001B[43m",
        BackgroundColor.Maroon: "\u001B[41m",
        BackgroundColor.Cyan: "\u001B[47m",
        BackgroundColor.White: "\u001B[46m"
    }[c]


class ChessBoard:
    """A structure that can have any fields defined."""

    def __init__(self, **entries): self.__dict__.update(entries)

    def toVector(self) -> List[Maybe[Piece]]:
        raise Exception("unimplemented")

    def highlights(self) -> List[Maybe[BackgroundColor]]:
        raise Exception("unimplemented")

    def nextPlayer(self) -> Color:
        raise Exception("unimplemented")


def switch(cb: ChessBoard) -> ChessBoard:
    return ChessBoard(toVector=lambda: cb.toVector(),
                      highlights=lambda: cb.highlights(),
                      nextPlayer=lambda: other(cb.nextPlayer()))


def initialPosition() -> ChessBoard:
    rearRow = [PieceType.Rook, PieceType.Knight, PieceType.Bishop, PieceType.Queen, PieceType.King, PieceType.Bishop,
               PieceType.Knight, PieceType.Rook]
    whiteRearRow = list(map(lambda typ: Just(Piece(Color.WHITE, typ)), rearRow))
    whiteFrontRow = [Just(Piece(Color.WHITE, PieceType.Pawn)) for i in range(0, 8)]
    emptyRow = [Nothing for i in range(0, 8)]

    blackFrontRow = [Just(Piece(Color.BLACK, PieceType.Pawn)) for i in range(0, 8)]
    blackRearRow = list(map(lambda typ: Just(Piece(Color.BLACK, typ)), rearRow))

    cells = whiteRearRow + whiteFrontRow + \
            concat([emptyRow for i in range(0, 4)]) + \
            blackFrontRow + blackRearRow

    return ChessBoard(toVector=lambda: cells,
                      highlights=lambda: [Nothing for i in range(0, 64)],
                      nextPlayer=lambda: Color.WHITE)


def showChessBoard(cb: ChessBoard) -> str:
    columnHeader = "".join(mapl(lambda x: f" {x} ", charRangeInclusive('a', 'h')))
    rowHeader = [f" {i + 1} " for i in range(0, 8)]
    alternateBlackWhite = concat([concat([[BackgroundColor.White, BackgroundColor.Cyan] for i in range(0, 4)]) +
                                  concat([[BackgroundColor.Cyan, BackgroundColor.White] for i in range(0, 4)])
                                  for i in range(0, 4)
                                  ])

    colorCodes = list(zip(cb.highlights(), alternateBlackWhite))
    cells = list(zip(colorCodes, cb.toVector()))

    cells2d = list(chunks(cells, 8))

    def showLine(highlightCol_bgCol_piece: Tuple[Tuple[Maybe[BackgroundColor], BackgroundColor], Maybe[Piece]]):
        (colors, piece) = highlightCol_bgCol_piece
        (highlightCol, bgCol) = colors
        return ansiCode(highlightCol.orElse(bgCol)) + " " + str(piece.orElse(" ")) + " " + ANSI_RESET

    lines = mapl(lambda lineCells: "".join(mapl(showLine, lineCells)), cells2d)

    def concatTuples(xy: Tuple[str, str]):
        return str(xy[0]) + str(xy[1]) + str(xy[0])

    return f"   {columnHeader}\n" + "\n".join(mapl(concatTuples, zip(rowHeader, lines))) + f"\n   {columnHeader}"


# print(showChessBoard(initialPosition()))


def at(cb: ChessBoard, pos: Position) -> Maybe[Piece]:
    i = toIndex(pos)
    if i < 0 or i > 63:
        return Nothing
    return cb.toVector()[i]


def cellIsEmpty(cb: ChessBoard, pos: Position):
    return at(cb, pos) == Nothing


def diffColor(cb: ChessBoard, x: Position, y: Position) -> bool:
    def match(px: Piece, py: Piece):
        (colX, typX) = px
        (colY, typY) = py
        return colX != colY

    return at(cb, x).map(lambda posX:
                         at(cb, y).map(lambda posY: match(posX, posY)).orElse(False)
                         ).orElse(False)


def movesRecursion(cb: ChessBoard, start: Position, here: Position, fn: Callable) -> List[Position]:
    nextt = fn(here)
    if not valid(nextt):
        return []
    if cellIsEmpty(cb, nextt):
        return [nextt] + movesRecursion(cb, start, nextt, fn)
    if diffColor(cb, start, nextt):
        return [nextt]
    return []


def movesFrom(cb: ChessBoard, here: Position, fn: Callable) -> List[Position]:
    return movesRecursion(cb, here, here, fn)


def up(pos: Position) -> Position:
    (i, j) = pos
    return i - 1, j


def down(pos: Position) -> Position:
    (i, j) = pos
    return i + 1, j


def left(pos: Position) -> Position:
    (i, j) = pos
    return i, j - 1


def right(pos: Position) -> Position:
    (i, j) = pos
    return i, j + 1


def diagUpLeft(pos: Position) -> Position:
    (i, j) = pos
    return i - 1, j - 1


def diagUpRight(pos: Position) -> Position:
    (i, j) = pos
    return i - 1, j + 1


def diagDownLeft(pos: Position) -> Position:
    (i, j) = pos
    return i + 1, j - 1


def diagDownRight(pos: Position) -> Position:
    (i, j) = pos
    return i + 1, j + 1


def rookMoves(cb: ChessBoard, pos: Position) -> List[Position]:
    return movesFrom(cb, pos, up) + \
           movesFrom(cb, pos, down) + \
           movesFrom(cb, pos, left) + \
           movesFrom(cb, pos, right)


def take(n: int, items: Iterable):
    return [x for _, x in zip(range(n), items)]


def kingMoves(cb: ChessBoard, pos: Position) -> List[Position]:
    return take(1, movesFrom(cb, pos, up)) + \
           take(1, movesFrom(cb, pos, down)) + \
           take(1, movesFrom(cb, pos, left)) + \
           take(1, movesFrom(cb, pos, right)) + \
           take(1, movesFrom(cb, pos, diagUpLeft)) + \
           take(1, movesFrom(cb, pos, diagUpRight)) + \
           take(1, movesFrom(cb, pos, diagDownLeft)) + \
           take(1, movesFrom(cb, pos, diagDownRight))


def bishopMoves(cb: ChessBoard, pos: Position) -> List[Position]:
    return movesFrom(cb, pos, diagUpLeft) + \
           movesFrom(cb, pos, diagUpRight) + \
           movesFrom(cb, pos, diagDownLeft) + \
           movesFrom(cb, pos, diagDownRight)


def queenMoves(cb: ChessBoard, pos: Position) -> List[Position]:
    return rookMoves(cb, pos) + bishopMoves(cb, pos)


AND = Infix(lambda f, g: (lambda x: f(x) and g(x)))


def knightMoves(cb: ChessBoard, pos: Position) -> List[Position]:
    fn = lambda y: diffColor(cb, pos, y) or cellIsEmpty(cb, y)

    return filterl(valid |AND| fn,
                   [compose(up, up, left)(pos)]
                   + [compose(up, up, right)(pos)]
                   + [compose(down, down, left)(pos)]
                   + [compose(down, down, right)(pos)]
                   + [compose(left, left, up)(pos)]
                   + [compose(left, left, down)(pos)]
                   + [compose(right, right, up)(pos)]
                   + [compose(right, right, down)(pos)]
                   )


def pawnMovesNonCapture(cb: ChessBoard, pos: Position, dirFn: Callable, movingForTheFirstTime: Callable) -> List[Position]:
    isEmpty = lambda p: cellIsEmpty(cb, p)

    nextt = dirFn(pos)
    nextToNext = compose(dirFn, dirFn)(pos)

    if movingForTheFirstTime(pos):
        return [nextt for _ in range(0, 1) if isEmpty(nextt)] + \
               [nextToNext for _ in range(0, 1) if isEmpty(nextt) and isEmpty(nextToNext)]

    return [nextt for _ in range(0, 1) if isEmpty(nextt)]


def pawnMovesDown(cb: ChessBoard, pos: Position) -> List[Position]:
    return pawnMovesNonCapture(cb, pos, down, lambda p: p[0] == 1) + \
           filterl(lambda y: diffColor(cb, pos, y), [diagDownLeft(pos)]) + \
           filterl(lambda y: diffColor(cb, pos, y), [diagDownRight(pos)])


def pawnMovesUp(cb: ChessBoard, pos: Position) -> List[Position]:
    return pawnMovesNonCapture(cb, pos, up, lambda p: p[0] == 6) + \
           filterl(lambda y: diffColor(cb, pos, y), [diagUpLeft(pos)]) + \
           filterl(lambda y: diffColor(cb, pos, y), [diagUpRight(pos)])


def possiblePositionsToMove(cb: ChessBoard, pos: Position) -> List[Position]:
    def getMoves(piece: Piece):
        (col, typ) = piece
        return {
            PieceType.Rook: lambda: rookMoves(cb, pos),
            PieceType.Knight: lambda: knightMoves(cb, pos),
            PieceType.Bishop: lambda: bishopMoves(cb, pos),
            PieceType.King: lambda: kingMoves(cb, pos),
            PieceType.Queen: lambda: queenMoves(cb, pos),
            PieceType.Pawn: lambda: pawnMovesDown(cb, pos) if col == Color.WHITE else pawnMovesUp(cb, pos)
        }.get(typ, lazy([]))()

    return at(cb, pos).map(getMoves) \
        .orElse([])


def showPossibleMoves(pos: Maybe[Position], cb: ChessBoard) -> ChessBoard:
    def highlightedCells(p: Position):
        cells = mapl(lambda x: toIndex(x), possiblePositionsToMove(cb, p))
        return [Just(BackgroundColor.Yellow) if x in cells else Nothing for x in range(0, 64)]

    return pos.map(lambda p: ChessBoard(toVector=lambda: cb.toVector(),
                                        highlights=lambda: highlightedCells(p),
                                        nextPlayer=lambda: cb.nextPlayer())) \
        .orElse(cb)


def movePiece(posX: Position, posY: Position, cb: ChessBoard) -> ChessBoard:
    def updatedCells(x: int, y: int, pieces: List[Maybe[Piece]]):
        return updatedList(pieces, [(y, pieces[x]), (x, Nothing)])

    return ChessBoard(toVector=lambda: updatedCells(toIndex(posX), toIndex(posY), cb.toVector()),
                      highlights=lambda: [Nothing for _ in range(0, 64)],
                      nextPlayer=lambda: other(cb.nextPlayer()))


def validateAndMakeMove(posX: Maybe[Position], posY: Maybe[Position], cb: ChessBoard) -> Either[List[str], ChessBoard]:
    @curry
    def ifValidTargetFinallyMoveFrom(px: Position, py: Position):
        ys = mapl(lambda p: toIndex(p), possiblePositionsToMove(cb, px))
        return Right(movePiece(px, py, cb)) if toIndex(py) in ys else Left(["This move not allowed!"])

    @curry
    def ifMyTurn(px: Position, piece: Piece):
        (col, typ) = piece
        if not (col == cb.nextPlayer()):
            return Left(["Not your turn!"])

        return posY\
                .map(ifValidTargetFinallyMoveFrom(px))\
                .orElse(Left([f"Target position not given"]))

    def maybeMoveFromSource(x: Position):
        return at(cb, x).map(ifMyTurn(x)).orElse(Left(["No piece at given position"]))

    return posX.map(maybeMoveFromSource) \
        .orElse(Left(["No source position given!"]))
