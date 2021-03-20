package chess

import arrow.core.Either

open class GameCommands

data class Pick(val position: Position): GameCommands()

data class Move(val fromPosition: Position, val toPosition: Position): GameCommands()

data class Promotion(val position: Position, val pieceType: PieceType): GameCommands()

//parameter x here is not required for our purposes, but Kotlin doesn't allow data class without parameter
data class ShowBoard(val x: Int): GameCommands()

fun parseInput(str: String?): Either<String, GameCommands> {
    if(str == null || str.trim().isEmpty()) {
        return Either.right(ShowBoard(1))
    }
    val args = str.split(" ").map { it.trim() }
    if(args[0] == "promote") {
        return parsePromoteCommand(args.drop(1))
    }
    if(args.size == 1) {
        return parsePickCommand(args[0])
    }
    if(args.size == 2) {
        return parseMoveCommand(args[0], args[1])
    }
    return Either.left("Unrecognised command")
}

fun parseMoveCommand(x: String, y: String): Either<String, GameCommands> {
    val p1 = fromString(x)
    val p2 = fromString(y)
    if(p1 == null || p2 == null) {
        return Either.left("invalid position. If you are trying to move a pice give two positions separaated by space e.g a1 a2")
    }
    return Either.right(Move(p1, p2))
}

fun parsePickCommand(posStr: String): Either<String, GameCommands> {
    val position = fromString(posStr)
    if(posStr.isEmpty() || position == null) {
        return Either.left("invalid position. If you want to highlight the moves from a position, give just a position e.g. a1")
    }
    return Either.right(Pick(position))
}

fun parsePromoteCommand(args: List<String>): Either<String, GameCommands> {
    if(args.size != 2) {
        return Either.left("If you are trying to promote a pawn, type promote <position> <piece name>")
    }
    val p = fromString(args[0])
    val piece = when(args[1]) {
        "rook" -> PieceType.Rook
        "queen" -> PieceType.Queen
        "bishop" -> PieceType.Bishop
        "knight" -> PieceType.Knight
        else -> null
    }
    if(p == null) {
        return Either.left("Invalid position. First argument to promote must be a position e.g. a1")
    }
    if(piece == null) {
        return Either.left("Invalid piece. You can promote to queen, rook, bishop, or knight")
    }
    return Either.right(Promotion(p, piece))
}
