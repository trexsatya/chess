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


main :: IO ()
main = game Nothing initialPosition
--main = putStrLn ""

clearScreen :: IO ()
clearScreen = do
  _ <- SP.system "clear"
  return ()


game :: Maybe String -> ChessBoard -> IO ()
game msg cb = do
   if isNothing msg
    then  clearScreen >> print cb;
    else pure()

   if isJust msg
     then print msg
     else pure()

   putStr $ "input " ++ (if CB.nextPlayer cb == C.White then "WHITE" else "BLACK") ++ " > "
   hFlush stdout
   cmd <- getLine
   execute cb cmd


execute :: ChessBoard -> String -> IO ()
execute _ "quit" = putStrLn "Bye!"
execute cb str = do
          let cmd =  parseInput str
          case cmd of
            Left err -> game (Just err) cb
            Right cmd -> 
                  case result of
                     Validation (Left errs) -> game (Just (concat errs)) cb
                     Validation (Right cb') -> game Nothing cb'
                     where { result = case cmd of
                              Pick pos -> Validation $ Right (showPossibleMoves (Just pos) cb)
                              Move x y -> validateAndMakeMove (Just x) (Just y) cb
                              Promote pos piece -> Validation $ Left ["unimplemented"]
                              ShowBoard -> Validation $ Right cb
                      }
