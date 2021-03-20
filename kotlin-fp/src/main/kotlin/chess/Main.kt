import arrow.core.Either
import chess.*
import kotlin.system.exitProcess

fun game(msg: String?, cb: ChessBoard) {
    if(msg == null) {
        clearScreen()
        println(showChessBoard(cb))
    } else {
        println(msg)
    }

    print("input ${if(cb.nextPlayer(cb) == Color.WHITE) "WHITE" else "BLACK"} > ")
    execute(cb, readLine())
}

fun execute(cb: ChessBoard, cmdStr: String?) {
    if(cmdStr == "quit") {
        println("Bye!")
        exitProcess(0)
    }
    when(val cmd = parseInput(cmdStr)) {
        is Either.Left -> game(cmd.a, cb)
        is Either.Right -> {
            val result = when(cmd.b) {
                is Pick -> Either.right(showPossibleMoves((cmd.b as Pick).position, cb))
                is Move -> validateAndMakeMove((cmd.b as Move).fromPosition, (cmd.b as Move).toPosition, cb)
                is Promotion -> Either.left(listOf("unimplemented"))
                else -> Either.right(cb)
            }
            when(result) {
                is Either.Left -> game(result.a.joinToString(" "), cb)
                is Either.Right -> game(null, result.b)
            }
        }
    }
}

fun clearScreen() {
    Runtime.getRuntime().exec("clear")
}

fun main() {
    game(null, initialPosition())
}