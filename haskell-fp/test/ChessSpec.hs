module ChessSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Chess.Position as P
import Data.Maybe
import Data.Vector as V 
import Chess.ChessBoard as CB 
import Chess.Color
import Chess.GameCommands

spec :: Spec
spec = do
  describe "Chess.Rules.MoveCalculation" $ do
    it "returns the string representation of Position" $ do
      showPosition (fromString "a1") `shouldBe` "a1"

    it "returns the matrix position from string" $ do
      fromString "e8" `shouldBe` Just (7, 4)

    it "returns invalid for position outside board" $ do
      valid (9, 7) `shouldBe` False
      valid (7, 9) `shouldBe` False

    it "returns valid positions for Rook at given position" $ do
      let cb' = boardForTesting [((2, 0), Piece White Rook), 
                                 ((0, 0), Piece White Pawn),
                                 ((3, 0), Piece White Rook)]
      
      cellIsEmpty cb' (0, 0) `shouldBe` False
      cellIsEmpty cb' (1, 0) `shouldBe` True
      diffColor cb' (2, 0) (0, 0) `shouldBe` False

      let cb = boardForTesting [((4, 3), Piece White Rook)]
--      print cb
      -- validate that all positions up, down, left, right are returned
      rookMoves cb (2, 0) `shouldBe` [(1,0),(0,0),(3,0),(4,0),(5,0),(6,0),(7,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7)]
      
    it "returns valid positions for King at given position" $ do
      let cb' = boardForTesting [((2, 0), Piece White Rook), 
                                 ((0, 0), Piece White Pawn),
                                 ((3, 0), Piece White Rook)]
      kingMoves cb' (3, 5) `shouldBe` [(2,5),(4,5),(3,4),(3,6),(2,4),(2,6),(4,4),(4,6)]

    it "returns valid positions for Queen at given position" $ do
      let cb' = boardForTesting [((2, 0), Piece White Rook),
                                     ((0, 0), Piece White Pawn),
                                     ((3, 0), Piece White Rook)]
      queenMoves cb' (4, 3) `shouldBe` [(3,3),(2,3),(1,3),(0,3),(5,3),(6,3),(7,3),(4,2),(4,1),(4,0),(4,4),(4,5),(4,6),(4,7),(3,2),(2,1),(1,0),(3,4),(2,5),(1,6),(0,7),(5,2),(6,1),(7,0),(5,4),(6,5),(7,6)]

    it "returns valid positions for Knight at given position" $ do
      let cb' = boardForTesting [((2, 0), Piece White Rook),
                                           ((0, 0), Piece White Pawn),
                                           ((3, 0), Piece White Rook)]
      knightMoves cb' (4, 3) `shouldBe` [(2,2),(2,4),(6,2),(6,4),(3,1),(5,1),(3,5),(5,5)]
      
      let cb = boardForTesting []
      knightMoves cb (0, 1) `shouldBe` [(2,0),(2,2),(1,3)]

    it "returns valid positions for Pawn on top side" $ do
      let cb = boardForTesting []
      pawnMovesDown cb (1, 0) `shouldBe` [(2,0), (3,0)]
      pawnMovesDown cb (2, 0) `shouldBe` [(3,0)]

      let cb' = boardForTesting [((2, 1), Piece White Pawn), ((3, 2), Piece Black Pawn), ((3, 0), Piece Black Pawn)]
      pawnMovesDown cb' (2, 1) `shouldBe` [(3,1), (3, 0), (3, 2)]
      
      
    -- it "returns the first element of an *arbitrary* list" $
    --   property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException

    it "returns parsed command from terminal" $ do
      parseInput "  " `shouldBe` Right ShowBoard
      parseInput "12" `shouldBe` Left "invalid position. If you want to highlight the moves from a position, give just a position e.g. a1"
      parseInput " a1" `shouldBe` Right (Pick (0, 0))
      parseInput " a1   a2 " `shouldBe` Right (Move (0, 0) (1, 0))
      parseInput " promote " `shouldBe` Left "If you are trying to promote a pawn, type promote <position> <piece name>"
      parseInput "promote asd asd" `shouldBe` Left "Invalid position. First argument to promote must be a position e.g. a1"
      parseInput "promote a1 asd" `shouldBe` Left "Invalid piece. You can promote to queen, rook, bishop, or knight"
      parseInput "promote a1 rook" `shouldBe` Right (Promote (0,0) Rook)

    it "returns valid positions for Bishop at given position" $ do
      let cb' = boardForTesting [((4, 4), Piece Black Bishop), 
                                 ((0, 0), Piece White Pawn), 
                                 ((2, 2), Piece White Rook)]
      print $ showPossibleMoves (Just (4, 4)) cb'                          
      pawnMovesDown cb' (2, 1) `shouldBe` [(3,1)]