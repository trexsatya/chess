from app.chess.ChessBoard import ChessBoard, showChessBoard, showPossibleMoves, initialPosition, validateAndMakeMove
from app.chess.Color import Color
from app.chess.GameCommands import parseInput, Pick, Move, Promotion
from app.chess.utils import Maybe, Nothing, fromNullable, when, Left, Right, Just, _, lazy
from os import system
import sys


def clearScreen():
    print(chr(27) + "[2J")
    # system("clear")
    sys.stderr.write("\x1b[2J\x1b[H")


def execute(cb: ChessBoard, inpStr: Maybe[str]):
    if inpStr.flat_map(lambda x: x) == "quit":
        exit(1)
    cmd = parseInput(inpStr)

    if isinstance(cmd, Left):
        game(cmd.value, cb)
    elif isinstance(cmd, Right):
        result = when(cmd.value, {
            # This is a hack! Python doesn't have switch statements or any elegant form of case matching
            lambda x: isinstance(x, Pick): lambda: Right(showPossibleMoves(Just(cmd.value.position), cb)),
            lambda x: isinstance(x, Move): lambda: validateAndMakeMove(Just(cmd.value.fromPos), Just(cmd.value.toPos), cb),
            lambda x: isinstance(x, Promotion): Left("Unimplementted"),
            _: Right(cb)
        })
        if isinstance(result, Left):
            game(result.value, cb)
        else:
            game(Nothing, result.value)


def game(msg: Maybe[str], cb: ChessBoard):
    if msg == Nothing:
        clearScreen()
        print(showChessBoard(cb))
    else:
        print(msg)

    print(f"input {'WHITE' if cb.nextPlayer() == Color.WHITE else 'BLACK'} > ", end="")
    execute(cb, fromNullable(input()))


if __name__ == '__main__':
    game(Nothing, initialPosition())
