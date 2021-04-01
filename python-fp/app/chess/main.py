from typing import List

from toolz import curry

from app.chess.ChessBoard import ChessBoard, showChessBoard, showPossibleMoves, initialPosition, validateAndMakeMove
from app.chess.Color import Color
from app.chess.GameCommands import parseInput, Pick, Move, Promotion, GameCommands
from app.chess.utils import Maybe, Nothing, fromNullable, when, Left, Right, Just, _, lazy, is_, Either
from os import system
import sys


def clearScreen():
    print(chr(27) + "[2J")
    system("clear")
    sys.stderr.write("\x1b[2J\x1b[H")


@curry
def executeCommand(cb: ChessBoard, cmd: GameCommands) -> Either[List[str], ChessBoard]:
    return when(cmd, {
        # This is a hack! Python doesn't have switch statements or any elegant form of case matching
        is_(Pick): lambda: Right(showPossibleMoves(Just(cmd.position), cb)),
        is_(Move): lambda: validateAndMakeMove(cmd, cb),
        is_(Promotion): Left("Unimplemented"),
        _: Right(cb)
    })


def game(msg: Maybe[str], cb: ChessBoard):
    if msg == Nothing:
        clearScreen()
        print(showChessBoard(cb))
    else:
        print(msg)

    print(f"input {'WHITE' if cb.nextPlayer() == Color.WHITE else 'BLACK'} > ", end="")

    [msg, cb_] = parseInput(fromNullable(input())).flat_map(executeCommand(cb))\
                                                           .either(lambda l: [Just(" ".join(l)), cb],
                                                                   lambda r: [Nothing, r])

    game(msg, cb_)


if __name__ == '__main__':
    game(Nothing, initialPosition())
