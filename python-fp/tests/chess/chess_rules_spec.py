from typing import List, Tuple
from unittest import TestCase

from app.chess.ChessBoard import Piece, ChessBoard, PieceType, cellIsEmpty, diffColor, rookMoves, showPossibleMoves, \
    showChessBoard
from app.chess.Color import Color
from app.chess.Position import Position, toIndex, showPosition, fromString
from app.chess.utils import Nothing, updatedList, mapl, Just


def boardForTesting(pieces: List[Tuple[Position, Piece]]):
    return ChessBoard(toVector=lambda: updatedList([Nothing for x in range(0, 64)], mapl(lambda pp: (toIndex(pp[0]),
                                                                                                     Just(pp[1])),
                                                                                         pieces)),
                      highlights=lambda: [Nothing for _ in range(0, 64)],
                      nextPlayer=lambda: Color.WHITE)


def piece(x, y, col, typ):
    return (x, y), Piece(col, typ)


class TestChessRulesSpec(TestCase):
    def test_returns_position_mapping_from_file_rank(self):
        self.assertEqual("a1", showPosition(fromString("a1")))
        self.assertEqual((7, 4), fromString("e8").orElse(()))

    def test_returns_valid_positions_for_Rook_at_given_position(self):
        cb = boardForTesting([
            piece(2, 0, Color.WHITE, PieceType.Rook),
            piece(0, 0, Color.WHITE, PieceType.Pawn),
            piece(3, 0, Color.WHITE, PieceType.Rook)
        ])

        self.assertEqual(False, cellIsEmpty(cb, (0, 0)))
        self.assertEqual(False, cellIsEmpty(cb, (2, 0)))
        self.assertEqual(True , cellIsEmpty(cb, (1, 0)))
        self.assertEqual(False, diffColor(cb, (2, 0), (0, 0)))

        cb = boardForTesting([
            piece(0, 0, Color.WHITE, PieceType.Rook),
            piece(0, 1, Color.WHITE, PieceType.Knight),
            piece(1, 0, Color.BLACK, PieceType.Pawn),
            piece(1, 3, Color.WHITE, PieceType.Pawn)
        ])

        self.assertEqual(False, diffColor(cb, (0, 1), (1, 3)))
        self.maxDiff = None

        print(showChessBoard(showPossibleMoves(Just((0, 0)), cb)))

        # self.assertCountEqual([(1,0), (0,0),(3,0),(4,0),(5,0),(6,0),(7,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7)],
        #                       rookMoves(cb, (2, 0)))

    def test_returns_valid_positions_for_Bishop_at_given_position(self):
        cb = boardForTesting([
            piece(4, 4, Color.BLACK, PieceType.Bishop),
            piece(0, 0, Color.WHITE, PieceType.Pawn),
            piece(2, 2, Color.WHITE, PieceType.Rook)
        ])

        print(showChessBoard(showPossibleMoves(Just((4, 4)), cb)))