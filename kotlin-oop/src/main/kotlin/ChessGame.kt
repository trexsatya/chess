import arrow.core.Either
import arrow.core.getOrElse
import java.lang.Exception

enum class Color { BLACK, WHITE }

class Cell(var x: Int, var y: Int) { /**[KOTLIN GUIDE] This is declaration of a class, along with its Primary Constructor. var => variable, var x: Int means a variable x of type Int (i.e. integer). */

    companion object { /**[KOTLIN GUIDE]  This is what you have in place of static methods in Java classes. */
        /**
         * Returns Cell if indexes are in boundary of a board. //TODO: Should this be moved to Board class, as it works on constraints of Board.
         */
        //[KOTLIN GUIDE] This is how you define function/method, pay attention return type is after parameter list.
        fun at(x: Int, y: Int) : Cell? {
            if(x < 0 || x > 7 || y < 0 || y > 7) return null
            return Cell(x,y)
        }

        /**
         * Takes a string like "a5" and returns a Cell with zero-indexed row number and column number, or error string
         */
        //[KOTLIN GUIDE] Either is a functional programming Monad, which is helpful if you want to gracefully handle error scenarios. Either<X,Y> means X=error, Y=success-value. See <a href="https://arrow-kt.io/docs/arrow/core/either/"></a>
        fun at(string: String) : Either<String,Cell> {

            //[KOTLIN GUIDE] Using regex in Kotlin is much easier.
            //[KOTLIN GUIDE] val means value; which is final, cannot be reassigned once assigned.
            val matchResult = "([a-h])(\\d)+".toRegex().matchEntire(string)

            val chars = listOf("a","b", "c", "d", "e", "f", "g", "h")

            //[KOTLIN GUIDE] This elvis statement allows some clarity.
            //[KOTLIN GUIDE] ?. is null-safe operator, the expression after .? will be executed only if the value is not-null. extractedValues will become null if matchResult is null or matchResult.groupValues is null
            val extractedValues = matchResult?.groupValues?.takeLast(2) ?: return Either.left("Could not get cell for $string. Try something like a4, b1")

            try {
                val row = Integer.parseInt(extractedValues[1])
                if(row < 1 || row > 8) return Either.left("Incorrect row number:$row")

                return Either.right(Cell( row -1, chars.indexOf(extractedValues[0])))

            } catch (e: Exception){
                return Either.left(e.toString())
            }
        }
    }

    //[KOTLIN GUIDE] This is how you override functions from parent class. Notice how we have to use 'override' keyword explicitly.
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        //[KOTLIN GUIDE] This is smart caste. After this line you don't have to cast explicitly. You get the casted object automatically.
        //[KOTLIN GUIDE] Also if you check type using this <code> x is String </code>, you don't have to cast after this.
        other as Cell

        if (x != other.x) return false
        if (y != other.y) return false

        return true
    }

    override fun hashCode(): Int {
        var result = x
        result = 31 * result + y
        return result
    }

    override fun toString(): String {
        val columns = listOf("a","b", "c", "d", "e", "f", "g", "h")

        //[KOTLIN GUIDE] String templating. You don't have to concatenate variables. You can directly use variables/expressions inside string.
        return "${columns[y]}${x+1}"
    }

}

abstract class Piece(val color: Color) {
    //[KOTLIN GUIDE] Board? means that board value can be null. If you use Board instead of Board? here, Kotlin won't allow you to assign null value.
    //[KOTLIN GUIDE] One of biggest advantages of Kotlin is that it forces you to think about null-safe design.
    var board: Board? = null

    /**
     * Useful for calculating valid cells to move.
     */
    //[KOTLIN GUIDE] This is how you declare lambda functions
    val getCellsTillFirstNonEmpty = { from: Cell, rowDir : Int, colDir: Int ->

        //[KOTLIN GUIDE] Kotlin also forces you to design for immutability, by default collections in Kotlin are immutable, you explicitly have to tell
        // that you want mutable collection.
        val returnList: MutableList<Cell> = mutableListOf()
        var i = 1
        while (true) {
            val nextCell = Cell.at(from.x + i*rowDir, from.y + i*colDir) ?: break

            returnList += nextCell
            if(board!!.pieceAt(nextCell) != null ) {
                break
            }
            i++
        }

        //[KOTLIN GUIDE] Don't need to use 'return' keyword. Actually you can't use.
        returnList
    }

    //[KOTLIN GUIDE] You have to explicitly tell that this function is open to be overridden by classes. It more explicitly forces you to think about open-close principle of SOLID.
    //[KOTLIN GUIDE] By default classes and functions are closed (final)
    open fun calculateTargetSquares(from: Cell) : List<Cell?> {
        return emptyList()
    }

    fun possibleTargetSquares(from: Cell): List<Cell> {
        return calculateTargetSquares(from)
            .filterNotNull()
            //[KOTLIN GUIDE] If your lambda has only one parameter, you don't need to declare it, you get it by default with name 'it'.
            //[KOTLIN GUIDE] board!! means that you are explicitly telling Kotlin that board cannot be null at this point, if it is Kotlin will throw NullPointerException.
            //[KOTLIN GUIDE] The declared type of board is Board? instead of Board, hence Kotlin wants you to either use ?. (null safe operator to access object) or !! to tell that it cannot be null.
            .filter { board!!.pieceAt(it)?.color != this.color } //Cannot capture own's color!
    }
}

class Pawn(color: Color) : Piece(color) {
    var forwardStep = { x: Int -> if(color == Color.WHITE) -x else +x}
    var backwardStep = { x: Int -> if(color == Color.WHITE) +x else -x}

    /**
     * Pawn has special moves to capture opponent's pieces.
     */
    private fun capturableCells(from: Cell): List<Cell?> {
        return listOf(Cell.at(from.x + forwardStep(1), from.y + 1), Cell.at(from.x + forwardStep(1), from.y - 1))
            .filter { board!!.pieceAt(it) != null }
    }

    private fun forwardMoves(from: Cell): List<Cell> {
        val firstMove = from.x == 1 || from.x == 6
        if(firstMove) {
            return listOf(Cell(from.x + forwardStep(1), from.y), Cell(from.x + forwardStep(2), from.y))
                .takeWhile { board!!.pieceAt(it) == null }
        } else {
            return listOf(Cell(from.x + forwardStep(1), from.y))
                .takeWhile { board!!.pieceAt(it) == null }
        }
    }

    override fun calculateTargetSquares(from: Cell): List<Cell> {
        //[KOTLIN GUIDE] Kotlin has concept of operator overloading like C++ however limited. It provides some operators overloaded for typical use-cases e.g. addition to collection (list etc)
       return forwardMoves(from) + capturableCells(from).filterNotNull()
    }

    override fun toString(): String {
        //[KOTLIN GUIDE] Instead of ternary opertor you have to use this.
        return if(color == Color.BLACK) "♟" else  "♙"
    }
}

class Knight(color: Color) : Piece(color) {

    override fun calculateTargetSquares(from: Cell): List<Cell?> {
        val x = from.x
        val y = from.y

        return listOf(Cell.at(x+2, y-1), Cell.at(x+2, y+1),
                      Cell.at(x-2, y-1), Cell.at(x-2, y+1),
                      Cell.at(x-1, y+2), Cell.at(x+1, y+2),
                      Cell.at(x-1, y-2), Cell.at(x+1, y-2))

    }

    override fun toString(): String {
        return if(color == Color.BLACK) "♞" else "♘"
    }
}

class Bishop(color: Color) : Piece(color) {
    override fun toString(): String {
        return if(color == Color.BLACK) "♝" else "♗"
    }

    override fun calculateTargetSquares(from: Cell): List<Cell?> {
        return getCellsTillFirstNonEmpty(from, +1, +1) +
        getCellsTillFirstNonEmpty(from, -1, -1) +
        getCellsTillFirstNonEmpty(from, +1, -1) +
        getCellsTillFirstNonEmpty(from, -1, +1)

    }
}

class Rook(color: Color) : Piece(color) {
    override fun toString(): String {
        return if(color == Color.BLACK) "♜" else "♖"
    }

    override fun calculateTargetSquares(from: Cell): List<Cell?> {
        return getCellsTillFirstNonEmpty(from, 0, +1) +
                getCellsTillFirstNonEmpty(from, 0, -1) +
                getCellsTillFirstNonEmpty(from, +1, 0) +
                getCellsTillFirstNonEmpty(from, -1, 0)
    }
}

class Queen(color: Color) : Piece(color) {
    override fun toString(): String {
        return if(color == Color.BLACK) "♛" else "♕"
    }

    override fun calculateTargetSquares(from: Cell): List<Cell?> {
        return getCellsTillFirstNonEmpty(from, +1, +1) +
                getCellsTillFirstNonEmpty(from, -1, -1) +
                getCellsTillFirstNonEmpty(from, +1, -1) +
                getCellsTillFirstNonEmpty(from, -1, +1) +
                getCellsTillFirstNonEmpty(from, 0, +1) +
                getCellsTillFirstNonEmpty(from, 0, -1) +
                getCellsTillFirstNonEmpty(from, +1, 0) +
                getCellsTillFirstNonEmpty(from, -1, 0)
    }
}

class King(color: Color) : Piece(color) {
    override fun toString(): String {
        return if(color == Color.BLACK) "♚" else "♔"
    }

    override fun calculateTargetSquares(from: Cell): List<Cell?> {
        return getCellsTillFirstNonEmpty(from, +1, +1).take(1) +
                getCellsTillFirstNonEmpty(from, -1, -1).take(1) +
                getCellsTillFirstNonEmpty(from, +1, -1).take(1) +
                getCellsTillFirstNonEmpty(from, -1, +1).take(1) +
                getCellsTillFirstNonEmpty(from, 0, +1).take(1) +
                getCellsTillFirstNonEmpty(from, 0, -1).take(1) +
                getCellsTillFirstNonEmpty(from, +1, 0).take(1) +
                getCellsTillFirstNonEmpty(from, -1, 0).take(1)
    }
}


class Board() {
    var selectedCell: Cell? = null
    var cellsToHighlight: List<Cell> = emptyList()

    fun render() {
        val ANSI_RED_BACKGROUND = "\u001B[41m";
        val ANSI_YELLOW_BACKGROUND = "\u001B[43m";
        val ANSI_CYAN_BACKGROUND = "\u001B[46m";
        val ANSI_WHITE_BACKGROUND = "\u001B[47m";

        val ANSI_RESET = "\u001B[0m";

        val backgrounds = arrayOf(ANSI_CYAN_BACKGROUND, ANSI_WHITE_BACKGROUND)

        var backgroundIndex = 0

        val columns = listOf(" ","a ","b ", "c ", "d", "e ", "f ", "g ", "h ")

        println(columns.joinToString(" "))
        val EMPTY_CELL = "\u3000"

        //[KOTLIN GUIDE] You get easy syntax for looping on range. The following means for(int i=0; i <= 7; i++)
        for (i in 0..7) {
            //[KOTLIN GUIDE] You can convert range to list and apply functional operations like filter, map etc
            val row = (0..7).toList().map {j ->
                val cell = Cell(i, j)
                val piece = cellToPieceMap[cell]
                var backgroundColor = backgrounds[backgroundIndex++%2]

                if(cell.equals(selectedCell) || cell in cellsToHighlight) backgroundColor = ANSI_YELLOW_BACKGROUND

                backgroundColor + ( piece ?: EMPTY_CELL)+" "+ANSI_RESET

            }.joinToString("")

            println("${i+1} "+row + " "+(i+1))
            backgroundIndex++
        }
        println(ANSI_RESET+columns.joinToString(" "))

    }

    private var cellToPieceMap: MutableMap<Cell?, Piece> = mutableMapOf()

    //[KOTLIN GUIDE] To perform something during construction, you have to use init {..} There is constructor(param-list){} available for Secondary constructors.
    init {
        val placePiece = { it: String, piece: Piece -> cellToPieceMap[Cell.at(it).getOrElse { null }] = piece; piece.board = this }

        listOf("a1", "h1").forEach { placePiece(it, Rook(Color.BLACK)) }
        listOf("a8", "h8").forEach { placePiece(it,  Rook(Color.WHITE)) }
        listOf("a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2").forEach { placePiece(it,  Pawn(Color.BLACK)) }
        listOf("a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7").forEach { placePiece(it,  Pawn(Color.WHITE)) }

        listOf("b1", "g1").forEach { placePiece(it,  Knight(Color.BLACK)) }
        listOf("b8", "g8").forEach { placePiece(it,  Knight(Color.WHITE)) }

        listOf("c1", "f1").forEach { placePiece(it,  Bishop(Color.BLACK)) }
        listOf("c8", "f8").forEach { placePiece(it,  Bishop(Color.WHITE)) }

        listOf("d8").forEach { placePiece(it,  Queen(Color.WHITE)) }
        listOf("e8").forEach { placePiece(it,  King(Color.WHITE)) }


        listOf("d1").forEach { placePiece(it,  Queen(Color.BLACK)) }
        listOf("e1").forEach { placePiece(it,  King(Color.BLACK)) }

    }

    private fun isCheckMate(){

    }

    private fun isStaleMate() {

    }

    private fun isDraw(){

    }

    fun pieceAt(cell: Cell?): Piece? {
        return this.cellToPieceMap[cell]
    }

    fun setPieceAt(cell: Cell, piece: Piece) {
        this.cellToPieceMap[cell] = piece
    }

    fun removePiece(cell: Cell) {
        this.cellToPieceMap.remove(cell)
    }
}


class Player(private var name: String) {

    override fun toString(): String {
        return name
    }
}

enum class MoveType {
    NORMAL, EN_PASSANT, PROMOTION
}

class Move (val piece: Piece, val from: Cell, val to: Cell, var moveType: MoveType = MoveType.NORMAL) {
    var capturedPiece: Piece? = null
    var capturedCell: Cell? = null

    init {
        if(piece.color == Color.WHITE && to.x == 0) moveType = MoveType.PROMOTION
        if(piece.color == Color.BLACK && to.x == 7) moveType = MoveType.PROMOTION
    }

    fun algebraicNotation(): String {
        return "" //TODO
    }
}

class ChessGame(var playerA: Player, var playerB: Player) {
    var isRunning = false
    var moves: MutableList<Move> = mutableListOf()

    private var turn: Int = 0
    val colorA = Color.WHITE
    val colorB = Color.BLACK

    private val capturedPieces: Map<Color, MutableList<Piece>> = mapOf(
        Pair(Color.WHITE, mutableListOf()),
        Pair(Color.BLACK, mutableListOf())
    )

    //which color for which player? TODO

    val board: Board = Board()

    /**
     * One-time opportunity for pawn to capture opposite pawn.
     */
    fun enPassantMove(cell: Cell): Move? {
        val lastMove = moves.lastOrNull() ?: return null

        val pawnToCapture = lastMove.piece
        val positionOfCapture = lastMove.to

        if(pawnToCapture is Pawn && Math.abs(positionOfCapture.x - lastMove.from.x) == 2 && //was starting move of pawn && was 2-step move
            (cell.x == positionOfCapture.x) &&  //on same level
            Math.abs(cell.y - positionOfCapture.y) == 1) { //on adjacent column

            val move = Move(
                board.pieceAt(cell)!!,
                cell,
                Cell.at(positionOfCapture.x + pawnToCapture.backwardStep(1), positionOfCapture.y)!!,
                MoveType.EN_PASSANT
            )
            move.capturedPiece = pawnToCapture
            move.capturedCell = positionOfCapture

            return move
        }

        return null
    }

    private fun validCellsToMove(sourceCell: Cell) : Either<String, List<Cell>> {
        val selectedPiece = board.pieceAt(sourceCell) ?: return Either.left("No piece at $sourceCell")

        val selectedColor = selectedPiece.color
        if(selectedColor != getTurn()) return Either.left("You cannot pick $selectedColor")

        val possibleTargetSquares = selectedPiece.possibleTargetSquares(sourceCell)
        if(possibleTargetSquares.isEmpty()) return Either.left("Nowhere to go! Select some other square!")
        return Either.right(possibleTargetSquares)
    }

    fun makeMove(move: Move) : Piece? {
        val targetCell = move.to
        val sourceCell = move.from

        var capturedPiece = board.pieceAt(targetCell)
        val sourcePiece = board.pieceAt(sourceCell)

        board.setPieceAt(targetCell,  sourcePiece!!)
        board.removePiece(sourceCell)

        if(move.moveType == MoveType.EN_PASSANT) {
           board.removePiece(move.capturedCell!!)
            capturedPiece = move.capturedPiece
        }

        move.capturedPiece = capturedPiece
        moves.plusAssign(move)

        val piecesCapturedByThisColor = capturedPieces[getTurn()]!!

        if(capturedPiece != null) {
            //[KOTLIN GUIDE] To add to the mutable collection.
            piecesCapturedByThisColor.plusAssign(capturedPiece)
        }

        board.selectedCell = null
        board.cellsToHighlight = emptyList()

        return capturedPiece
    }

    fun getTurn(): Color {
        return if(turn == 0) Color.WHITE else Color.BLACK;
    }

    fun run() {
        if(isRunning) return

        isRunning = true
        while (true) {
            this.board.render()
            val move = getAValidMove(this.board)
            val capturedPieces = this.makeMove(move)

            if(capturedPieces != null) {
                println("You now have bagged: $capturedPieces")
            }

            turn = (turn+1) % 2
        }
    }

    private fun getAValidMove(board: Board): Move {
        while (true) {
            println("select square [${this.getTurn()}] >")
            val sourceCellCmd: String = readLine().orEmpty()

            if(Cell.at(sourceCellCmd).isLeft()) {
                println("Invalid cell! Choose something like a8, b4 etc!")
                continue
            }

            val source = Cell.at(sourceCellCmd).get()
            val cellsToMove = this.validCellsToMove(source)

            val enPassant: Move? = this.enPassantMove(source)

            if(cellsToMove.isLeft() && enPassant == null) {
                println("Invalid move! $cellsToMove")
                continue
            }

            board.selectedCell = source
            var possibleTargetCells: List<Cell> = cellsToMove.fold( {x -> emptyList() },  { x -> x} )

            if(enPassant != null) {
                possibleTargetCells += enPassant.to
            }

            board.cellsToHighlight = possibleTargetCells
            board.render()

            while (true) {
                println("Type 'undo' to start over your move. Where do you want to move? Choose from $possibleTargetCells")
                val targetCellCmd = readLine().orEmpty()

                if(targetCellCmd == "undo") {
                    return getAValidMove(board)
                }

                val targetCell = Cell.at(targetCellCmd)
                if(targetCell.isLeft()) continue

                if(enPassant != null && targetCell.get() == enPassant.to) {
                    return enPassant
                }

                if(targetCell.get() in possibleTargetCells) {
                    return Move(board.pieceAt(source)!!, source, Cell.at(targetCellCmd).get())
                }
            }
        }
    }

}


fun main(args: Array<String>) {
    val me = Player("me")
    val you = Player("you")

    val game = ChessGame(me, you)
    game.run()
}
