{-# LANGUAGE TupleSections #-}

module Main(main) where

import Chess.ChessBoard as CB
import qualified System.Process as SP
-- import Move
-- import ChessRules
import Data.Maybe
--import Data.Text as T
--import Text.Regex
import Chess.Color as C
import Chess.Position as P
-- import Decisions
import System.IO (hFlush, stdout)
import Text.Read
-- import System.Console.ANSI as A
import Data.List.Split
import Debug.Trace
import Chess.GameCommands
import Data.Char (isSpace)
import Control.Monad (when)


main :: IO ()
main =  game (Just "Type a position to highlight the possible valid moves from that position. \
                                   \Give two positions separated by space to move a piece. \
                                   \Use Type promote <pos1> <pos2> to promote a piece. Press Enter to start") initialPosition
--main = putStrLn ""

clearScreen :: IO ()
clearScreen = do
  _ <- SP.system "clear"
  return ()


game :: Maybe String -> ChessBoard -> IO ()
game msg cb = do
   when (isNothing msg) (clearScreen >> print cb)
   when (isJust msg) (print msg)

   putStr $ "input " ++ (if CB.nextPlayer cb == C.White then "WHITE" else "BLACK") ++ " > "
   hFlush stdout
   str <- getLine

--  let (err_msg, cb') = either (\left -> (Just(concat left), cb)) (\right -> (Nothing, right)) (parseInput str >>= executeCmd cb)
--  The following is same as the above: Using TupleSection for conciseness 
   let (err_msg, cb') = either (\left -> (Just(concat left), cb)) (Nothing,) (parseInput str >>= executeCmd cb)
   game err_msg cb'

-- Haskell's function definition-level pattern matching makes programs more concise, and clean

executeCmd :: ChessBoard -> Command -> Either [String] ChessBoard
executeCmd cb ShowBoard = Right cb
executeCmd cb (Pick x) = Right (showPossibleMoves (Just x) cb)
executeCmd cb (Move x y) = validateAndMakeMove (Just x) (Just y) cb
executeCmd _ (Promote _ _) = Left ["unimplemented"]

