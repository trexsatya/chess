module Chess.GameCommands where

import Data.Char (isSpace)
import Chess.Position
import Chess.ChessBoard
import Data.Maybe
import Data.List.Split
import Debug.Trace

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

data Command = Move Position Position| Pick Position| Promote Position PieceType| ShowBoard deriving (Show, Eq)

parsePickCommand:: String -> Either [String] Command
parsePickCommand str | isNothing p =  Left ["invalid position. If you want to highlight the moves from a position, give just a position e.g. a1"]
                     | otherwise = Right (Pick (fromJust p))
                      where {p = fromString str}

parseMoveCommand:: String -> String -> Either [String] Command
parseMoveCommand str1 str2 | isNothing p1 || isNothing p2 =  Left ["invalid position. If you are trying to move a pice give two positions separaated by space e.g a1 a2"]
                     | otherwise = Right (Move (fromJust p1) (fromJust p2))
                      where {p1 = fromString str1; p2 = fromString str2}

parsePromoteCommand:: [String] -> Either [String] Command
parsePromoteCommand  args | length args /= 2 = Left ["If you are trying to promote a pawn, type promote <position> <piece name>"]
                     | isNothing p =  Left ["Invalid position. First argument to promote must be a position e.g. a1"]
                     | isNothing piece = Left ["Invalid piece. You can promote to queen, rook, bishop, or knight"]
                     | otherwise = Right (Promote (fromJust p) (fromJust piece))
                      where {p = fromString (head args);
                             piece = case args !! 1 of
                                             "rook" -> Just Rook
                                             "queen" -> Just Queen
                                             "bishop" -> Just Bishop
                                             "knight" -> Just Knight
                                             _ -> Nothing
                               }


parseInput :: String -> Either [String] Command
parseInput [] = Right ShowBoard
parseInput str = let { args = filter (/= []) $ map trim $ splitOn " " $ trim str; n = length args;} in
                   let cmd | null (trim str) = Right ShowBoard
                           | head args == "promote" = parsePromoteCommand (drop 1 args)
                           | n == 1 = parsePickCommand (head args)
                           | n == 2 = parseMoveCommand (head args) (args !! 1)
                           | otherwise = Left ["Unrecognised command"] in
                    cmd

