package chess

import arrow.core.Either
import arrow.core.Predicate
import chess.BackgroundColor.*
import chess.Color.BLACK
import chess.Color.WHITE
import chess.PieceType.*
import kotlin.reflect.KFunction1

enum class PieceType {
    Pawn, Knight, Bishop, Rook, Queen, King;
}

data class Piece(val color: Color, val type: PieceType) {
    override fun toString(): String {
        if (color == WHITE) {
            return when (type) {
                Pawn -> "♙"
                Knight -> "♘"
                Bishop -> "♗"
                Rook -> "♖"
                Queen -> "♕"
                King -> "♔"
            }
        } else {
            return when (type) {
                Pawn -> "♟"
                Knight -> "♞"
                Bishop -> "♝"
                Rook -> "♜"
                Queen -> "♛"
                King -> "♚"
            }
        }
    }
}

enum class BackgroundColor {
    Yellow { override fun toString(): String { return "\u001B[43m" } },
    Maroon { override fun toString(): String { return "\u001B[41m" } },
    White() { override fun toString(): String { return "\u001B[47m" } },
    Cyan() { override fun toString(): String { return "\u001B[46m" } }
}

interface ChessBoard {
    //    Transforms a ChessBoard into a 'Vector' of square (either populated
//    or empty), index 0 being square a1 and index 63 being square h8.
    fun toVector(): List<Piece?>
    fun highlights(): List<BackgroundColor?>
    fun nextPlayer(): Color
}

fun initialPosition(): ChessBoard {
    return object : ChessBoard {
        override fun toVector(): List<Piece?> {
            val rearRow = listOf(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)
            val whiteRearRow = rearRow.map { Piece(WHITE, it) }
            val whiteFrontRow = (1..8).map { Piece(WHITE, Pawn) }
            val emptyRow = (1..8).map { null }
            val blackFrontRow = (1..8).map { Piece(BLACK, Pawn) }
            val blackRearRow = rearRow.map { Piece(BLACK, it) }

            return whiteRearRow + whiteFrontRow + (1..4).map { emptyRow }.flatten() + blackFrontRow + blackRearRow
        }
        override fun highlights(): List<BackgroundColor?> { return (1..64).map { null } }
        override fun nextPlayer(): Color { return WHITE }
    }
}

//-- | Change player making the next move.
fun switch(cb: ChessBoard): ChessBoard {
    return object : ChessBoard {
        override fun toVector(): List<Piece?> { return cb.toVector() }
        override fun highlights(): List<BackgroundColor?> { return cb.highlights() }
        override fun nextPlayer(): Color { return other(cb.nextPlayer()) }
    }
}

const val ANSI_RESET = "\u001B[0m"

fun showChessBoard(cb: ChessBoard): String {
    val columnHeader = ('a'..'h').map { " $it " }.joinToString("")
    val rowHeader = (1..8).map { " $it " }
    val alternateBlackWhite = (1..4).map {
        (1..4).map { listOf(White, Cyan) }.flatten() +
                (1..4).map { listOf(Cyan, White) }.flatten()
    }.flatten()

    val colorCodes = cb.highlights().zip(alternateBlackWhite)
    val cells = colorCodes.zip(cb.toVector())
    val cells2d = cells.chunked(8)
    val showLine = { highlightCol_bgCol_piece: Pair<Pair<BackgroundColor?, BackgroundColor>, Piece?> ->
        val (colors, piece) = highlightCol_bgCol_piece
        val (highlightCol, bgCol) = colors

        val lineStr = (highlightCol ?: bgCol).toString() + " " + (piece ?: " ") + " "+ ANSI_RESET
        lineStr
    }

    val lines = cells2d.map { it.map(showLine).joinToString("") }

    val concatTuples = { it: Pair<String, String> -> it.first + it.second + it.first }

    return "   $columnHeader\n" + rowHeader.zip(lines).map { concatTuples(it) }.joinToString("\n") + "\n   $columnHeader\n"
}

fun at(cb: ChessBoard, pos: Position): Piece? {
    val i = toIndex(pos)
    if (i < 0 || i > 63) return null
    return cb.toVector()[i]
}

fun cellIsEmpty(cb: ChessBoard, pos: Position): Boolean {
    return at(cb, pos) == null
}

fun diffColor(cb: ChessBoard, x: Position, y: Position): Boolean {
    val pieceX = at(cb, x)
    val pieceY = at(cb, y)
    if(pieceX == null || pieceY == null) return false

    val (colX, typX) = pieceX
    val (colY, _) = pieceY

    return colX != colY
}

fun moveRecursion(cb: ChessBoard, start: Position, here: Position, fn: KFunction1<Position, Position>): List<Position> {
    val next = fn.invoke(here)
    if(!valid(next)) return listOf()
    if(cellIsEmpty(cb, next)) return listOf(next) + moveRecursion(cb, start, next, fn)
    if(diffColor(cb, start, next)) return listOf(next)
    return listOf()
}

fun from(cb: ChessBoard, here: Position, fn: KFunction1<Position, Position>): List<Position> {
    return moveRecursion(cb, here, here, fn)
}

fun up(pos: Position): Position {
    val (i, j) = pos
    return Pair(i-1, j)
}

fun down(pos: Position): Position {
    val (i, j) = pos
    return Pair(i+1, j)
}

fun left(pos: Position): Position {
    val (i, j) = pos
    return Pair(i, j-1)
}

fun right(pos: Position): Position {
    val (i, j) = pos
    return Pair(i, j+1)
}

fun diagUpLeft(pos: Position): Position {
    val (i, j) = pos
    return Pair(i-1, j-1)
}

fun diagUpRight(pos: Position): Position {
    val (i, j) = pos
    return Pair(i-1, j+1)
}

fun diagDownLeft(pos: Position): Position {
    val (i, j) = pos
    return Pair(i+1, j-1)
}

fun diagDownRight(pos: Position): Position {
    val (i, j) = pos
    return Pair(i+1, j+1)
}


fun rookMoves(cb: ChessBoard, pos: Position): List<Position> {
    return from(cb, pos, ::up) +
            from(cb, pos, ::down) +
            from(cb, pos, ::left) +
            from(cb, pos, ::right)
}

fun kingMoves(cb: ChessBoard, pos: Position): List<Position> {
    return from(cb, pos, ::up).take(1) +
            from(cb, pos, ::down).take(1) +
            from(cb, pos, ::left).take(1) +
            from(cb, pos, ::right).take(1) +
            from(cb, pos, ::diagUpLeft).take(1) +
            from(cb, pos, ::diagUpRight).take(1) +
            from(cb, pos, ::diagDownLeft).take(1) +
            from(cb, pos, ::diagDownRight).take(1)
}

fun queenMoves(cb: ChessBoard, pos: Position): List<Position> {
    return rookMoves(cb, pos) + bishopMoves(cb, pos)
}

fun bishopMoves(cb: ChessBoard, pos: Position): List<Position> {
    return from(cb, pos, ::diagUpLeft) +
            from(cb, pos, ::diagUpRight) +
            from(cb, pos, ::diagDownLeft) +
            from(cb, pos, ::diagDownRight)
}

//https://www.harivignesh.dev/kotlin-creative-function-composition-with-extensions-and-operator-overloading

infix fun <T, U, V> ((T) -> U).then(other: (U) -> V): (T) -> V = { other(this(it)) }

fun knightMoves(cb: ChessBoard, pos: Position): List<Position> {
    val cells = (listOf((::up then ::up then ::left)(pos)) +
                 listOf((::up then ::up then ::right)(pos)) +
                 listOf((::down then ::down then ::left)(pos)) +
                 listOf((::down then ::down then ::right)(pos)) +
                 listOf((::left then ::left then ::up)(pos)) +
                 listOf((::left then ::left then ::down)(pos)) +
                 listOf((::right then ::right then ::up)(pos)) +
                 listOf((::right then ::right then ::down)(pos))
                )
    return cells.filter { valid(it) }.filter { diffColor(cb, pos, it) || cellIsEmpty(cb, it) }
}

fun pawnMovesNonCapture(cb: ChessBoard, pos: Position, dirFn: KFunction1<Position, Position>, movingForFirstTime: Predicate<Position>): List<Position> {
    val isEmpty: (Position) -> Boolean = { p -> cellIsEmpty(cb, p)}

    val next  = dirFn.invoke(pos)
    val nextToNext = (dirFn then dirFn)(pos)

    if(movingForFirstTime(pos)) return listOf(next).filter { isEmpty(it) } +
            (if (isEmpty(next) && isEmpty(nextToNext)) listOf(nextToNext) else listOf())

    return listOf(next).filter { isEmpty(it) }
}

fun pawnMovesDown(cb: ChessBoard, pos: Position): List<Position> {
    return pawnMovesNonCapture(cb, pos, ::down, { it.first == 1}) +
            listOf(diagDownLeft(pos)).filter { diffColor(cb, pos, it) } +
            listOf(diagDownRight(pos)).filter { diffColor(cb, pos, it) }
}

fun pawnMovesUp(cb: ChessBoard, pos: Position): List<Position> {
    return pawnMovesNonCapture(cb, pos, ::up, { it.first == 6}) +
            listOf(diagUpLeft(pos)).filter { diffColor(cb, pos, it) } +
            listOf(diagUpRight(pos)).filter { diffColor(cb, pos, it) }
}

fun possiblePositionsToMove(cb: ChessBoard, pos: Position): List<Position> {
    val piece = at(cb, pos) ?: return listOf()

    val (col, typ) = piece
    return when (typ) {
        Rook -> rookMoves(cb, pos)
        Knight -> knightMoves(cb, pos)
        Bishop -> bishopMoves(cb, pos)
        King -> kingMoves(cb, pos)
        Queen -> queenMoves(cb, pos)
        Pawn -> when (col) {
            WHITE -> pawnMovesDown(cb, pos)
            BLACK -> pawnMovesUp(cb, pos)
        }
    }
}

fun <T> updatedList(list: List<T>, values: List<Pair<Int, T>>): List<T> {
    val ml = list.toMutableList()
    values.forEach {
        ml[it.first] = it.second
    }
    return ml.toList()
}

fun showPossibleMoves(pos: Position?, cb: ChessBoard): ChessBoard {
    if(pos == null) return cb

    return object: ChessBoard {
        override fun toVector(): List<Piece?> {
            return cb.toVector()
        }
        override fun highlights(): List<BackgroundColor?> {
            val highlighted = {cells: List<Int> -> (1..64).map { if (it in cells) Yellow else null}}
            val yellows = highlighted(possiblePositionsToMove(cb, pos).map { toIndex(it) + 1 })
            return updatedList(yellows, listOf(Pair(toIndex(pos), Maroon)))
        }
        override fun nextPlayer(): Color {
            return cb.nextPlayer()
        }
    }
}

fun movePiece(posX: Position?, posY: Position?, cb: ChessBoard): ChessBoard {
    if(posX == null || posY == null) return cb;
    return switch(object: ChessBoard {
        override fun toVector(): List<Piece?> {
            val cells = cb.toVector()
            return updatedList(cells, listOf(Pair(toIndex(posX), null),
                                             Pair(toIndex(posY), cells[toIndex(posX)])))
        }
        override fun highlights(): List<BackgroundColor?> {
            return (1..64).map { null }
        }
        override fun nextPlayer(): Color {
            return cb.nextPlayer()
        }
    })
}

fun validateAndMakeMove(fromPos: Position?, toPos: Position?, board: ChessBoard): Either<List<String>, ChessBoard>{
    if(fromPos == null || toPos == null) {
        return Either.left(listOf("Invalid position"))
    }
    val pickedPiece = at(board, fromPos)
    val y = toIndex(toPos)
    val ys = possiblePositionsToMove(board, fromPos).map { toIndex(it) }

    if(pickedPiece == null) {
        return Either.left(listOf("No piece at selected position"))
    }
    if(y !in ys) {
        return Either.left(listOf("Not allowed! ${y} not in ${ys}"))
    }
    val (color, typ) = pickedPiece
    if(color != board.nextPlayer()) {
        return Either.left(listOf("Not your turn!"))
    }
    return Either.right(movePiece(fromPos, toPos, board))
}
