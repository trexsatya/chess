package chess

import junit.framework.TestCase.assertEquals

import org.spekframework.spek2.Spek

object MyTest: Spek({
//    println("this is the root")
    fun boardForTesting(pieces: List<Pair<Position, Piece>>): ChessBoard {
        return object: ChessBoard {
            override fun toVector(cb: ChessBoard): List<Piece?> {
                val cells: List<Piece?> = (1..64).map { null }
                return updatedList(cells, pieces.map { Pair(toIndex(it.first), it.second) })
            }
            override fun highlights(cb: ChessBoard): List<BackgroundColor?> { return (1..64).map { null } }
            override fun nextPlayer(cb: ChessBoard): Color { return Color.WHITE }
        }
    }

    fun piece(x: Int, y: Int, c: Color, t: PieceType): Pair<Pair<Int, Int>, Piece> {
        return Pair(Pair(x, y), Piece(c, t))
    }

    group("Chess.Rules.MoveCalculation") {
        test("returns the string representation of Position") {
            assertEquals("a1", showPosition (fromString("a1") ))
        }

        test("returns the matrix position from string") {
            assertEquals(Pair(7,4), fromString("e8"))
        }

        test("returns valid positions for Rook at given position") {
            val cb = boardForTesting(listOf(
                piece(2, 0, Color.WHITE, PieceType.Rook),
                piece(0, 0, Color.WHITE, PieceType.Pawn),
                piece(3, 0, Color.WHITE, PieceType.Rook)
            ))
            assertEquals(false, cellIsEmpty(cb, Pair(0, 0)))
            assertEquals(false, cellIsEmpty(cb, Pair(2, 0)))
            assertEquals(true,  cellIsEmpty(cb, Pair(1, 0)))
            assertEquals(false, diffColor(cb, Pair(2, 0), Pair(0, 0)))

            val cb2 = boardForTesting(listOf(
                piece(4, 3, Color.WHITE, PieceType.Rook)
            ))
            assertEquals(listOf(Pair(1,0),Pair(0,0),Pair(3,0),Pair(4,0),Pair(5,0),Pair(6,0),Pair(7,0),Pair(2,1),Pair(2,2),Pair(2,3),Pair(2,4),Pair(2,5),Pair(2,6),Pair(2,7)),
                        rookMoves(cb2, Pair(2, 0))
            )
        }

        test("returns valid positions for Bishop at given position") {
            val cb = boardForTesting(listOf(
                piece(4, 4, Color.BLACK, PieceType.Bishop),
                piece(0, 0, Color.WHITE, PieceType.Pawn),
                piece(2, 2, Color.WHITE, PieceType.Rook)
            ))

            print(showChessBoard(showPossibleMoves(Pair(4, 4), cb)))
        }


    }

//    group("another group") {
//        println("another group")
//        test("another test") {
//            println("another test")
//        }
//    }
})