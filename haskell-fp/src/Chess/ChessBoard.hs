{-# LANGUAGE DeriveFunctor #-}

module Chess.ChessBoard (
     PieceType (..)
   , Piece (Piece)
   , ChessBoard(nextPlayer)
   , emptyBoard
   , initialPosition
   , showPossibleMoves
--   , movePiece
   , validateAndMakeMove
   , rookMoves
   , kingMoves
   , queenMoves
   , knightMoves
   , boardForTesting
   , cellIsEmpty
   , pawnMovesDown
   , diffColor
   , Validation(..)
   -- , valid
   ) where

import qualified Data.Char as C
import qualified Data.Vector as V
import Chess.Color
import Chess.Position as P
import Data.List (intersperse, intercalate)
import qualified Data.Vector.Unboxed as U
import Debug.Trace
import Data.Maybe

-- This just represents an enumeration of piece types
data PieceType =
             Pawn 
           | Knight
           | Bishop
           | Rook
           | Queen
           | King
   deriving (Eq, Show)

-- Here we have something that has a constructor, Peace on the RHS is constructor that takes Color, and PieceType
-- constructor declaration can take type variables BTW, just like anything else
data Piece = Piece !Color !PieceType
   deriving Eq

instance Show Piece where 
   show (Piece White t) =
      case t of
         Pawn -> "♙"
         Knight -> "♘"
         Bishop -> "♗"
         Rook -> "♖"
         Queen -> "♕"
         King -> "♔"
   show (Piece Black t) =
      case t of
         Pawn -> "♟"
         Knight -> "♞"
         Bishop -> "♝"
         Rook -> "♜"
         Queen -> "♛"
         King -> "♚"


data HighlightColor = Yellow | Maroon
   -- deriving (Show)

instance Show HighlightColor where
  show Yellow = "\ESC[43m\STX "
  show Maroon = "\ESC[105m\STX "

-- ! implies strict field

data ChessBoard = ChessBoard
   { -- | Transforms a ChessBoard into a 'Vector' of square (either populated
     -- or empty), index 0 being square a1 and index 63 being square h8.
     toVector :: !(V.Vector (Maybe Piece)),
     highlights :: !(V.Vector (Maybe HighlightColor)),
     -- | Player making the next move.
     nextPlayer :: !Color
   }

partition :: Int -> [a] -> [[a]]; partition _ [] = []; partition n xs = take n xs : partition n (drop n xs)

emptyBoard :: Color -> ChessBoard
emptyBoard firstPlayer = ChessBoard {
   toVector = V.replicate 64 Nothing,
   highlights = V.replicate 64 Nothing,
   nextPlayer = firstPlayer
 }

boardForTesting :: [(Position, Piece)] -- List of (position, piece) tuple
             -> ChessBoard
boardForTesting pieces = ChessBoard { 
   toVector = V.replicate 64 Nothing V.// placePieces,
   highlights = V.replicate 64 Nothing,
   nextPlayer = White
 } where
   placePieces = map (\pos_piece -> let i = toIndex (fst pos_piece); p = snd pos_piece in
                      (i, Just p)) pieces

initialPosition :: ChessBoard
initialPosition = ChessBoard {
     toVector =  V.fromList $ concat $
        [whiteRearRow, whiteFrontRow]
        ++
        replicate 4 emptyRow
        ++
        [blackFrontRow, blackRearRow]
     , 
     highlights = V.replicate 64 Nothing,
     -- highlights = V.fromList [if i `elem` [2, 3, 4, 5] then Just Yellow else Nothing | i <- [1..64]],
     nextPlayer = White
   }
   where
   whiteRearRow  = map (Just . Piece White) rearRow
   whiteFrontRow = replicate 8 $ Just $ Piece White Pawn
   emptyRow      = replicate 8 Nothing
--   List comprehension an also be used here. emptyRow = [Nothing | i <- [1..8]] 
   blackFrontRow = replicate 8 $ Just $ Piece Black Pawn
   blackRearRow  = map (Just . Piece Black) rearRow
   rearRow       = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- | Change player making the next move.
switch :: ChessBoard -> ChessBoard
switch cb = ChessBoard { toVector = toVector cb,
                          highlights = V.replicate 64 Nothing,
                          nextPlayer = other $ nextPlayer cb }

instance Show ChessBoard where
   show cb = 
         "   " ++ columnHeader ++ "\n" ++ 
         unlines (concatTuples $ zip rowHeader  (map showLine cells2d)) ++
         "   " ++ columnHeader 
      where 
         cells = zip (V.toList $ toVector cb) colorCodes
         colorCodes = zip (V.toList $ highlights cb) alternateWhiteBlack
         alternateWhiteBlack = concat $ replicate 4 $ concat (replicate  4 [1, 2]) ++ concat (replicate  4 [2, 1])
         showLine = concatMap (\c -> bgColor(snd c) ++ piece(fst c) ++ reset)
         bgColor (maybeHighlight, dc) = case maybeHighlight of 
            Nothing -> defaultCellColor dc
            Just h -> show h
         piece c = maybe " " show c
         defaultCellColor 1 = white
         defaultCellColor 2 = black   
         cells2d = partition 8 cells
         black = "\ESC[46m\STX "
         white = "\ESC[47m\STX "
         reset = " \ESC[m\STX"
         rowHeader =  map (\x -> " " ++ show(x) ++ " ") [1..8]
         columnHeader = concat $ map (\x -> " " ++ id([x]) ++ " ") ['a'..'h']  -- show([x]) will print like "a", "b", .. (withing quotes)
         concatTuples = map (\x -> fst x ++ snd x ++ fst x)

-- Print the numbers and figure out the logic for calculating the cells where a piece is allowed to move
-- printNumberedBoard = putStrLn $ unlines $ partition 24 $ concat $ map (\x -> (\y -> printf "%3s" y) . show $ x) [1..64]

-- takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
-- takeWhileInclusive _ [] = []
-- takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
--                                          else []

cellIsEmpty :: ChessBoard -> Position -> Bool
cellIsEmpty cb x  = case piece of 
                           Nothing -> True
                           otherwise -> False
                      where piece = at cb x

diffColor :: ChessBoard -> Position -> Position  -> Bool
diffColor cb x y  = case (pieceX, pieceY) of
                  (Just (Piece Black tx), Just (Piece White ty)) -> True
                  (Just (Piece White tx), Just (Piece Black ty)) -> True
                  otherwise -> False
                where {pieceX = at cb x; pieceY = at cb y;}

moveRecursion :: ChessBoard -> Position -> Position -> (Position -> Position) -> [Position]
moveRecursion  cb start here f | not (P.valid next) = []
                              | cellIsEmpty cb next = next : moveRecursion cb start next f
                              | diffColor cb start next = [next]
                              | otherwise = []
                              where { next = f here; }

from :: ChessBoard -> Position -> (Position -> Position) -> [Position]
from cb here = moveRecursion cb here here
         

-- trace (show here ++ " is " ++ show next ++" empty?" ++ show (cellIsEmpty cb next))
-- we can use trace to do print-style debugging

-- down :: ChessBoard -> Position -> [Position] 
-- down cb x = tillPieceFoundToCapture cb x $ filter (<=64) $ map (+x) [8,16..56] 

-- up   :: ChessBoard -> Int -> [Int]
-- up cb x = tillPieceFoundToCapture cb x $ filter (>=1) $ map (\i -> x-i) [8,16..56] 

up   :: Position -> Position
up (i,j) = (i-1, j)

down   :: Position -> Position
down (i,j) = (i+1, j)

left   :: Position -> Position
left (i,j) = (i, j-1)

right   :: Position -> Position
right (i,j) = (i, j+1)

diagUpLeft :: Position -> Position
diagUpLeft (i, j) = (i-1, j-1)

diagUpRight :: Position -> Position
diagUpRight (i, j) = (i-1, j+1)

diagDownLeft :: Position -> Position
diagDownLeft (i, j) = (i+1, j-1)

diagDownRight :: Position -> Position
diagDownRight (i, j) = (i+1, j+1)

rookMoves :: ChessBoard -> Position -> [Position]
rookMoves cb pos = from cb pos up ++
                   from cb pos down ++
                   from cb pos left ++
                   from cb pos right

kingMoves :: ChessBoard -> Position -> [Position]
kingMoves cb pos = take 1 (from cb pos up) ++
                   take 1 (from cb pos down) ++
                   take 1 (from cb pos left) ++
                   take 1 (from cb pos right) ++
                   take 1 (from cb pos diagUpLeft) ++
                   take 1 (from cb pos diagUpRight) ++
                   take 1 (from cb pos diagDownLeft) ++
                   take 1 (from cb pos diagDownRight)


queenMoves :: ChessBoard -> Position -> [Position]
queenMoves cb pos = rookMoves cb pos ++ bishopMoves cb pos

bishopMoves :: ChessBoard -> Position -> [Position]
bishopMoves cb pos = from cb pos diagUpLeft ++
                     from cb pos diagUpRight ++
                    from cb pos diagDownLeft ++
                    from cb pos diagDownRight

knightMoves :: ChessBoard -> Position -> [Position]
knightMoves cb pos = filter (\y -> diffColor cb pos y || cellIsEmpty cb y) $
                      filter valid $
                     [(up.up.left) pos] ++ [(up.up.right) pos] ++
                     [(down.down.left) pos] ++ [(down.down.right) pos] ++
                     [(left.left.up) pos] ++ [(left.left.down) pos] ++
                     [(right.right.up) pos] ++ [(right.right.down) pos]

pawnMovesNonCapture :: ChessBoard -> Position -> (Position -> Position) -> (Position -> Bool) -> [Position]
pawnMovesNonCapture cb pos dirFn movingForFirstTime
      | firsTime = takeIfEmpty (dirFn pos) ++
                                           [(dirFn.dirFn) pos | isEmpty (dirFn pos) && isEmpty ((dirFn.dirFn) pos)]
      | otherwise = takeIfEmpty (dirFn pos)
      where {firsTime = movingForFirstTime pos; takeIfEmpty = \x -> [x | isEmpty x]; isEmpty = cellIsEmpty cb}

pawnMovesDown :: ChessBoard -> Position -> [Position]
pawnMovesDown cb pos = pawnMovesNonCapture cb pos down (\x -> fst x == 1) ++
                      [x | x <- [diagDownLeft pos], diffColor cb pos x] ++
                      [x | x <- [diagDownRight pos], diffColor cb pos x]

pawnMovesUp :: ChessBoard -> Position -> [Position]
pawnMovesUp cb pos = pawnMovesNonCapture cb pos up (\x -> fst x == 6) ++
                      [x | x <- [diagUpLeft pos], diffColor cb pos x] ++
                      [x | x <- [diagUpRight pos], diffColor cb pos x]


at :: ChessBoard -> Position -> Maybe Piece
at cb p
   | i < 0 || i > 63 = Nothing
   | otherwise = toVector cb V.! i
   where i = toIndex p


highlighted :: [Int] -> V.Vector(Maybe HighlightColor)
highlighted cells =  V.fromList [ if i `elem` cells then Just Yellow else Nothing | i <- [1..64]]

possiblePositionsToMove :: Position -> ChessBoard -> [Position]
possiblePositionsToMove pos cb = do
       let piece = at cb pos;
       case piece of
             Just (Piece _ Rook) -> rookMoves cb pos
             Just (Piece _ Knight) -> knightMoves cb pos
             Just (Piece _ Bishop) -> bishopMoves cb pos
             Just (Piece _ Queen) -> queenMoves cb pos
             Just (Piece _ King) -> kingMoves cb pos
             Just (Piece White Pawn) -> pawnMovesDown cb pos
             Just (Piece Black Pawn) -> pawnMovesUp cb pos
             otherwise -> []

showPossibleMoves :: Maybe Position -> ChessBoard -> ChessBoard
showPossibleMoves Nothing cb = cb 
showPossibleMoves (Just pos) cb = ChessBoard { toVector = toVector cb,
                          highlights = highlighted (map (\p -> toIndex p + 1) (possiblePositionsToMove pos cb)) V.// [(toIndex pos, Just Maroon)]
                          ,                              
                          nextPlayer = nextPlayer cb
                    }

movePiece :: Maybe Position -> Maybe Position -> ChessBoard -> ChessBoard
movePiece Nothing y cb = cb
movePiece x Nothing cb = cb
movePiece (Just x) (Just y) cb = switch $ ChessBoard { 
                          toVector = let cells = toVector cb in
                            cells V.// [(toIndex x, Nothing), (toIndex y, cells V.! toIndex x)],
                          highlights = V.fromList $ replicate 64 Nothing
                          ,                              
                          nextPlayer = nextPlayer cb
                    }

--this is simply a redefinition of Either
--Introducing a newtype enables you to change how a type behaves.
--By convention Left of Either  is used for errors, and Right for the correct value
--https://blog.ploeh.dk/2018/11/05/applicative-validation/
newtype Validation e r = Validation (Either e r) deriving (Eq, Show, Functor)
--This allows composition of validation functions in order to return combined list of error messages.
instance Monoid m => Applicative (Validation m) where
  pure = Validation . pure
  Validation (Left x) <*> Validation (Left y) = Validation (Left (mappend x y))
  Validation f <*> Validation r = Validation (f <*> r)

validateAndMakeMove :: Maybe Position -> Maybe Position -> ChessBoard -> Validation [String] ChessBoard
validateAndMakeMove Nothing _ cb = Validation $ Left ["Invalid position"]
validateAndMakeMove _ Nothing _  = Validation $ Left ["Invalid position"]
validateAndMakeMove  fromPos  toPos cb =
        case pickedPiece of
          Nothing -> Validation $ Left ["No piece at selected position"]
          Just (Piece color _) ->
             let resp | y+1 `notElem` ys = Validation $ Left ["Not allowed! " ++ show(y+1) ++ " not in " ++ show ys]
                      | color /= nextPlayer cb = Validation $ Left ["Not your turn!"]
                      | otherwise = Validation $ Right (movePiece fromPos toPos cb)
             in
             resp
      where {ys = map ((+1).toIndex) (possiblePositionsToMove (fromJust fromPos) cb);
             y = toIndex (fromJust toPos);
             pickedPiece = at cb (fromJust fromPos)
      }
