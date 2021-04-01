{-# LANGUAGE TupleSections #-}

module Chess.GameCommands where

import Data.Char (isSpace)
import Chess.Position
import Chess.ChessBoard
import Data.Maybe
import Data.List.Split
import Debug.Trace
import Data.Bifunctor
--import Control.Either
--import Control.Error.Util

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

data Command = Move Position Position| Pick Position| Promote Position PieceType| ShowBoard deriving (Show, Eq)

parsePickCommand:: String -> Either [String] Command
parsePickCommand str | isNothing p =  Left ["invalid position. If you want to highlight the moves from a position, give just a position e.g. a1"]
                     | otherwise = Right (Pick (fromJust p))
                      where {p = fromString str}

combiner :: b -> Either a (b0 -> (b, b0));
combiner x = Right (x,)

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight a Nothing = Left a
maybeToRight a (Just x) = Right x

--parseMoveCommand:: String -> String -> Either [String] Command
--parseMoveCommand str1 str2 =  mapToRight (uncurry Move) combined_pos
--                      where { combined_pos = (maybeToRight ["Invalid source position!"] fromPos >>= combiner) <*>
--                                              maybeToRight ["Invalid target position"] toPos;
--                              mapToRight = second; fromPos = fromString str1; toPos = fromString str2
--                       }

-- The below is same as the above definition, but using the benefits of do keyword
parseMoveCommand:: String -> String -> Either [String] Command
parseMoveCommand str1 str2 =  let {note a = maybe (Left a) Right} in do
                         x <- note ["Invalid source position! You can move by giving two valid positions e.g. a1 a2"] (fromString str1)
                         y <- note ["Invalid target position! You can move by giving two valid positions e.g. a1 a2"] (fromString str2)
                         Right (Move x y)

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

