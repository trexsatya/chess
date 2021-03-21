import {List} from 'immutable';
import Record from 'dataclass';
import * as M from 'pattern-matching-ts/lib/match'
import { pipe } from 'fp-ts/lib/function'
// import * as O from 'fp-ts/lib/Option'
import { none, some, fromPredicate, Option, map, filter, getOrElse } from 'fp-ts/Option'
import {fromString, Position} from "./Position";
import {charRangeInclusive} from "./utils";
import {Either, left, right} from "fp-ts/Either";
import {BackgroundColor} from "./ChessBoard";


// class GameCommands {
//
// }

// class Pick extends Record<GameCommands> {
//     position: Position;
// }
//
// class Move extends Record<GameCommands> {
//     fromPos: Position;
//     toPos: Position;
// }


// const Pick = Record({ position:   })
// const Move = Record({ a: 1, b: 2 })

// const GameCommands: any = Pick | Move

interface Pick {
    readonly _tag: 'Pick'
    readonly position: Position
}

interface Move {
    readonly _tag: 'Move'
    readonly fromPosition: Position
    readonly toPosition: Position
}

interface ShowBoard {
    readonly _tag: 'ShowBoard'
}

//This just means wherever I am expecting GameCommands I can pass Pick, Move
type GameCommands = Pick | Move | ShowBoard



const pickCmd = (pos: Position): GameCommands => ({_tag: "Pick", position: pos})
const moveCmd = (from: Position, to: Position): GameCommands => ({_tag: 'Move', fromPosition: from, toPosition: to})
const showBoardCmd = (): GameCommands => ({_tag: "ShowBoard"})


//export declare const map: <A, B>(f: (a: A) => B) => (fa: Option<A>) => Option<B>

function trim(str: string) {
    return str.trim()
}

const splitIntoWords = (sep: string) => (str: string) => str.split(sep).map(it => it.trim()).filter(it => it != "")


function parsePickCommand(posStr: string) {
    return pipe(fromString(posStr),
        map(p => right(pickCmd(p))),
        getOrElse(()=> left("invalid position. If you want to highlight the moves from a position, give just a position e.g. a1"))
    )
}

function parseMoveCommand(fromPosStr: string, toPosStr: string) {
    let msg = "invalid position. If you are trying to move a pice give two positions separaated by space e.g a1 a2"
    function seeIfToPositionIsValid(x: Position) {
        return pipe(
            fromString(toPosStr),
            map(y => right(moveCmd(x, y))),
            getOrElse(()=> left(msg))
        )
    }

    return pipe(fromString(fromPosStr),
        map(x => seeIfToPositionIsValid(x)),
        getOrElse(()=> left(msg))
    )
}

export function parseInput(inputStr: Option<string>): Either<string, GameCommands> {
    function handleCommand(args: string[]) {
        if(args[0] == "promote") return left("Unimplemented")
        if(args.length == 1) return parsePickCommand(args[0])
        if(args.length == 2) return parseMoveCommand(args[0], args[1])
        return left("Unrecognised command")
    }

    //     .map(lst => lst.map(trim))
    return pipe(inputStr,
                  map(trim),
                  filter(x => x != ""),
                  map(splitIntoWords(" ")),
                  map(handleCommand),
                  getOrElse(() => right(showBoardCmd()))
    )
}


export {GameCommands, Move, Pick, pickCmd, moveCmd}