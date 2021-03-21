import {Color, other} from './Color'
import {filter, flatten, getOrElse, map, none, Option, some} from 'fp-ts/Option'
import {charRangeInclusive, range, updatedList} from "./utils";
import {chunksOf, replicate, zip} from "fp-ts/ReadonlyArray";
import {Position, toIndex, valid} from "./Position";
import {pipe} from 'fp-ts/lib/function'
import {takeLeft} from "fp-ts/Array";
import {Either, left as eitherLeft, right as eitherRight} from "fp-ts/Either";

export enum PieceType {
    Pawn = 1,
    Knight = 2,
    Bishop = 3,
    Rook = 4,
    Queen = 5,
    King = 6,
}

export interface Piece {
    readonly color: Color
    readonly type: PieceType
}

const showPiece = (piece: Piece): string => {
    let {color, type} = piece
    if(color == Color.White) {
        switch (type) {
            case PieceType.Pawn: return "♙"
            case PieceType.Knight: return "♘"
            case PieceType.Bishop: return "♗"
            case PieceType.Rook: return "♖"
            case PieceType.Queen: return  "♕"
            case PieceType.King: return "♔"
        }
    } else {
        switch (type) {
            case PieceType.Pawn: return "♟"
            case PieceType.Knight: return "♞"
            case PieceType.Bishop: return "♝"
            case PieceType.Rook: return "♜"
            case PieceType.Queen: return "♛"
            case PieceType.King: return "♚"
        }
    }
}

const ANSI_RESET = "\u001B[0m"

export enum BackgroundColor {
    Yellow = "\u001B[43m",
    Maroon = "\u001B[41m",
    Cyan = "\u001B[47m",
    White = "\u001B[46m",
}

export interface ChessBoard {
    toVector(): Option<Piece>[]
    highlights(): Option<BackgroundColor>[]
    nextPlayer(): Color
}

const piece = (color: Color, type: PieceType): Piece => ({color, type})

const noHighlight = (): Option<BackgroundColor>[] => range(0,63).map(_=> none)

export function initialPosition(): ChessBoard {
    const rearRow = [PieceType.Rook, PieceType.Knight, PieceType.Bishop, PieceType.Queen, PieceType.King, PieceType.Bishop,
        PieceType.Knight, PieceType.Rook]
    const whiteRearRow = rearRow.map(it => some(piece(Color.White, it)))
    const whiteFrontRow = range(0, 7).map(_ => some(piece(Color.White, PieceType.Pawn)))
    const emptyRow = range(0, 7).map(_ => none)

    const blackFrontRow = range(0, 7).map(_ => some(piece(Color.Black, PieceType.Pawn)))
    const blackRearRow = rearRow.map(it => some(piece(Color.Black, it)))

    const cells = whiteRearRow.concat(
        whiteFrontRow,
        range(0, 3).map(_ => emptyRow).flat(),
        blackFrontRow,
        blackRearRow
    )

    return {
        toVector(): Option<Piece>[] {
            return cells
        },
        highlights(): Option<BackgroundColor>[] {
            return noHighlight()
        },
        nextPlayer(): Color {
            return Color.White
        }
    }
}


export function showChessBoard(cb: ChessBoard): string {
    const columnHeader = charRangeInclusive('a', 'h').map(it => ` ${it} `).join("")
    const rowHeader = range(1, 8)
    const alternateBlackWhite = replicate(4,
                                range(0,3).map(_ => [BackgroundColor.White, BackgroundColor.Cyan])
                        .concat(range(0,3).map(_ => [BackgroundColor.Cyan,  BackgroundColor.White]))
    ).flat().flat()

    const colorCodes = zip(cb.highlights(), alternateBlackWhite)
    const cells = zip(colorCodes, cb.toVector())

    const cells2d  = chunksOf(8)(cells)

    function showLineCells(highlightCol_bgColor_piece: readonly [readonly [Option<BackgroundColor>, BackgroundColor], Option<Piece>]): string {
        const [color, piece] = highlightCol_bgColor_piece
        const [highlightCol, bgColor] = color
        return getOrElse(() => bgColor)(highlightCol) + " " + getOrElse(() => " ")(map(showPiece)(piece)) + " " + ANSI_RESET
    }

    const lines = cells2d.map((line) => line.map(showLineCells))

    const concatTuples = (xy: readonly [number, string[]]): string => xy[0] + " " + xy[1].join("") + " "+ xy[0]

    return `  ${columnHeader}\n${zip(rowHeader, lines).map(concatTuples).join("\n")}\n  ${columnHeader}`
}

function at(cb: ChessBoard, pos: Position): Option<Piece> {
    if(!valid(pos)) return none
    return cb.toVector()[toIndex(pos)]
}

export function cellIsEmpty(cb: ChessBoard, pos: Position): boolean {
    return at(cb, pos) == none
}

export function diffColor(cb: ChessBoard, x: Position, y: Position): boolean {
    function seeIfYIsValid(pieceX: Piece) {
        return pipe(
            at(cb, y),
            map(pieceY => pieceX.color != pieceY.color),
            getOrElse(()=> false)  //no piece at position y
        )
    }

    return getOrElse(()=> false)( //no piece at position x
        pipe(
            at(cb, x),
            map(seeIfYIsValid)
        ))
}


export function boardForTesting(pieces: Array<[Position, Piece]>): ChessBoard {
    const initial: Option<Piece>[] = range(0,63).map(_=> none)

    //UNSAFE: Mutable operation, used for testing only
    pieces.forEach(it => {
        initial[toIndex(it[0])] = some(it[1])
    })

    return {
        toVector(): Option<Piece>[] {
            return initial
        },
        highlights(): Option<BackgroundColor>[] {
            return noHighlight()
        },
        nextPlayer(): Color {
            return Color.White
        }
    }
}

function moveRecursion(cb: ChessBoard, start: Position, here: Position, fn: Function): Position[] {
    const next = fn(here)
    if(!valid(next)) return []
    if(cellIsEmpty(cb, next)) return [next].concat(moveRecursion(cb, start, next, fn))
    if(diffColor(cb, start, next)) return [next]
    return []
}

function movesFrom(cb: ChessBoard, here: Position, directionFn: Function) {
    return moveRecursion(cb, here, here, directionFn)
}

const up = (pos: Position): Position => { const [i, j] = pos; return [i-1, j]}
const down = (pos: Position): Position => { const [i, j] = pos; return [i+1, j]}
const left = (pos: Position): Position => { const [i, j] = pos; return [i, j-1]}
const right = (pos: Position): Position => { const [i, j] = pos; return [i, j+1]}
const diagUpLeft = (pos: Position): Position => { const [i, j] = pos; return [i-1, j-1]}
const diagUpRight = (pos: Position): Position => { const [i, j] = pos; return [i-1, j+1]}
const diagDownLeft = (pos: Position): Position => { const [i, j] = pos; return [i+1, j-1]}
const diagDownRight = (pos: Position): Position => { const [i, j] = pos; return [i+1, j+1]}

function rookMoves(cb: ChessBoard, pos: Position) {
    return movesFrom(cb, pos, up).concat(
        movesFrom(cb, pos, down),
        movesFrom(cb, pos, left),
        movesFrom(cb, pos, right)
    )
}

function kingMoves(cb: ChessBoard, pos: Position) {
    return takeLeft(1)(movesFrom(cb, pos, up)).concat(
        takeLeft(1)(movesFrom(cb, pos, down)),
        takeLeft(1)(movesFrom(cb, pos, left)),
        takeLeft(1)(movesFrom(cb, pos, right)),
        takeLeft(1)(movesFrom(cb, pos, diagUpLeft)),
        takeLeft(1)(movesFrom(cb, pos, diagUpRight)),
        takeLeft(1)(movesFrom(cb, pos, diagDownLeft)),
        takeLeft(1)(movesFrom(cb, pos, diagDownRight))
    )
}

function bishopMoves(cb: ChessBoard, pos: Position) {
    return movesFrom(cb, pos, diagUpLeft).concat(
        movesFrom(cb, pos, diagUpRight),
        movesFrom(cb, pos, diagDownLeft),
        movesFrom(cb, pos, diagDownRight),
    )
}

function queenMoves(cb: ChessBoard, pos: Position) {
    return rookMoves(cb, pos).concat(bishopMoves(cb, pos))
}

function knightMoves(cb: ChessBoard, pos: Position): Position[] {
    const fn = (y: Position) => diffColor(cb, pos, y) || cellIsEmpty(cb, y)

    return [pipe(pos, up, up, left)].concat(
        [pipe(pos, up, up, right)],
        [pipe(pos, down, down, left)],
        [pipe(pos, down, down, right)],
        [pipe(pos, left, left, up)],
        [pipe(pos, left, left, down)],
        [pipe(pos, right, right, up)],
        [pipe(pos, right, right, down)],
    ).filter(it => valid(it) && fn(it))
}

function pawnMovesNonCapture(cb: ChessBoard, pos: Position, dirFn: Function, movingForTheFirstTime: Function) {
    const isEmpty = (p: Position) => cellIsEmpty(cb, p)
    const next: Position = dirFn(pos)
    const nextToNext: Position = dirFn(next)

    if(movingForTheFirstTime(pos)){
        return [next].filter(isEmpty).concat(
            [nextToNext].filter(it => isEmpty(next) && isEmpty(it))
        )
    }

    return [next].filter(isEmpty)
}

function pawnMovesUp(cb: ChessBoard, pos: Position) {
    return pawnMovesNonCapture(cb, pos, up, (p: Position) => p[0] == 6).concat(
        [diagUpLeft(pos)].filter((y)=> diffColor(cb, pos, y)),
        [diagUpRight(pos)].filter((y: Position)=> diffColor(cb, pos, y))
    )
}

function pawnMovesDown(cb: ChessBoard, pos: Position) {
    return pawnMovesNonCapture(cb, pos, down, (p: Position) => p[0] == 1).concat(
        [diagDownLeft(pos)].filter((y)=> diffColor(cb, pos, y)),
        [diagDownRight(pos)].filter((y: Position)=> diffColor(cb, pos, y))
    )
}

//This is required for giving type hint; simply returning an empty array doesn't tell TS which type it is
function noMoves(): Position[] {
    return []
}

function possiblePositionsToMove(cb: ChessBoard, pos: Position): Position[] {
    function getPositions(piece: Piece): Position[] {
        const {color, type} = piece
        switch (type) {
            case PieceType.Bishop: return bishopMoves(cb, pos)
            case PieceType.Rook: return rookMoves(cb, pos)
            case PieceType.King: return kingMoves(cb, pos)
            case PieceType.Queen: return queenMoves(cb, pos)
            case PieceType.Knight: return knightMoves(cb, pos)
            case PieceType.Pawn: switch (color) {
                case Color.Black: return pawnMovesUp(cb, pos)
                case Color.White: return pawnMovesDown(cb, pos)
            }
        }
        return []
    }

    return getOrElse(noMoves)(pipe(
        at(cb, pos), map(getPositions)
    ))
}

export function showPossibleMoves(pos: Option<Position>, cb: ChessBoard): ChessBoard {
    function highlightedCells(p: Position): Option<BackgroundColor>[] {
        const cells = possiblePositionsToMove(cb, p).map(toIndex)
        return range(0, 63).map(x => cells.includes(x) ? some(BackgroundColor.Yellow) : none)
    }

    return pipe(pos,
               map(p => ({
                   toVector(): Option<Piece>[] {
                       return cb.toVector()
                   },
                   highlights(): Option<BackgroundColor>[] {
                       return highlightedCells(p)
                   },
                   nextPlayer(): Color {
                       return Color.White
                   }
               })),
               getOrElse(()=> cb))
}

export function movePiece(posX: Option<Position>, posY: Option<Position>, cb: ChessBoard): ChessBoard {
    function seeIfPosYIsValid(px: Position): ChessBoard {
        return pipe(posY,
                map(py => ({
                    toVector(): Option<Piece>[] {
                        const x = toIndex(px), y = toIndex(py);
                        return updatedList(cb.toVector(), [[x, none], [y, cb.toVector()[x]]])
                    },
                    highlights(): Option<BackgroundColor>[] {
                        return noHighlight()
                    },
                    nextPlayer(): Color {
                        return other(cb.nextPlayer())
                    }
                })),
                getOrElse(()=> cb)
            )
    }

    return pipe(posX,
            map(seeIfPosYIsValid),
            getOrElse(() => cb)
        )
}

export function validateAndMakeMove(posX: Option<Position>, posY: Option<Position>, cb: ChessBoard): Either<string[], ChessBoard> {
    const ifPlayersTurnMoveFrom = (x: Position) => (piece: Piece): Either<string[], ChessBoard> => {
        const {color, type} = piece
        if(color != cb.nextPlayer()) return eitherLeft(["Not your turn!"])

        const ys = possiblePositionsToMove(cb, x).map(it => toIndex(it))
        return pipe(posY,
                filter(py => ys.includes(toIndex(py))),
                map(py => {
                    return eitherRight(movePiece(posX, posY, cb))
                }),
                getOrElse(() => eitherLeft(["Target position not given or movement not allowed"]))
            )
    }

    function moveMaybePieceFromGivenPos(pos_piece: [Position, Option<Piece>]): Either<string[], ChessBoard> {
        return pipe(pos_piece[1],
            map(ifPlayersTurnMoveFrom(pos_piece[0])),
            getOrElse(()=> eitherLeft(["No piece at source position"]))
        )
    }

    return pipe(posX,
            map((px: Position): [Position, Option<Piece>] => [px, cb.toVector()[toIndex(px)]]),
            //Notice that type inference fails here, and we have to explicitly give return type from lambda
            map(moveMaybePieceFromGivenPos),
            getOrElse(() => eitherLeft(["No source position given"]))
        )
}


