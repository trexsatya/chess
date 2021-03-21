import {
    boardForTesting,
    cellIsEmpty,
    ChessBoard,
    diffColor, initialPosition,
    Piece,
    PieceType,
    showChessBoard,
    showPossibleMoves, validateAndMakeMove
} from "./ChessBoard";
import {Color} from "./Color";
import {Position} from "./Position";
import {fromNullable, none, Option, some} from "fp-ts/Option";
import readline from 'readline';
import {GameCommands, parseInput} from "./GameCommands";
import {Either, getOrElse, isLeft, left, right} from "fp-ts/Either";
import {pipe} from "fp-ts/function";
import * as M from "pattern-matching-ts/match";

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

function clearScreen() {

}


function execute(cb: ChessBoard, inputStr: Option<string>) {
    const cmd = parseInput(inputStr)
    if(isLeft(cmd)) {
        game(some(cmd.left), cb)
    } else {
         // This asks for exhaustive match, so it's helpful
         let matchCommand = (command: GameCommands) => pipe(command,
            M.matchW("_tag")({
                Pick: ({position}) => right(showPossibleMoves(some(position), cb)),
                Move: ({fromPosition, toPosition}) => validateAndMakeMove(some(fromPosition), some(toPosition), cb),
                ShowBoard: () => right(cb),
            }),
        )

        //@ts-ignore
        const msgCb: [Option<string>, ChessBoard] = pipe(matchCommand(cmd.right),
                M.matchW("_tag")({
                    Left: ({left}) => [some(left +""), cb],
                    Right: ({right}) => [none, right],
                    _: ():[Option<string>, ChessBoard] => [none, cb]
                })
            )

        game(msgCb[0], msgCb[1])
    }
}

function game(msg: Option<string>, cb: ChessBoard) {
    if(msg == none) {
        clearScreen()
        console.log(showChessBoard(cb))
    } else {
        console.log(msg)
    }
    rl.question(`input ${cb.nextPlayer() == Color.White ? 'WHITE': 'BLACK'} > `, (answer) => {
        execute(cb, some(answer))
    });
}


function piece(x: number, y: number, color: Color, type: PieceType): [Position, Piece] {
    return [[x, y], {color, type}]
}

const cb = boardForTesting([
    piece(0, 0, Color.White, PieceType.Rook),
    piece(0, 1, Color.White, PieceType.Knight),
    piece(1, 0, Color.Black, PieceType.Pawn),
    piece(1, 3, Color.White, PieceType.Pawn),
])

console.log(cellIsEmpty(cb,[0,0]))
console.log(cellIsEmpty(cb,[2,0]))
console.log(cellIsEmpty(cb,[1,0]))
console.log(diffColor(cb,[2,0], [0,0]))

// console.log(showChessBoard(showPossibleMoves(some([0, 1]), cb)))

game(none, initialPosition())