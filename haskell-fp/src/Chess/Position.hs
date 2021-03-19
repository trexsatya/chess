module Chess.Position (
   Position,
   fromIndex,
   fromString,
   toIndex,
   showPosition,
   valid) where

import Data.Char

-- | Square on a chess board under the form (file, row). Both components should
-- be in [0..7].
type Position = (Int, Int)
-- This means that we can use Position in place of (Int, Int) everywhere; kind of a synonym
-- It doesn't give a data constructor though
   
-- In Chess, column is called file, hence represented by 'f' hereon

fromIndex :: Int -> Position
fromIndex n = (n `mod` 8, n `quot` 8)

toIndex :: Position -> Int
toIndex (f, r) = 8 * f + r

-- | Check the validity of a Position, i.e. checks that both file and row are
-- in [0..7].
valid :: Position -> Bool
valid (f, r) = valid' f && valid' r
   where valid' x = x >= 0 && x <= 7

fromString :: String -> Maybe Position
fromString s = case s of  
      [f, r] | f >= 'a' && f <= 'h' -> Just (digitToInt r-1, fromEnum f-97) 
      otherwise -> Nothing 
    -- where s = (l:n)

showPosition :: Maybe Position -> String
showPosition Nothing = ""
showPosition (Just (f,r)) = (['a'..'h'] !! r):"" ++ show (r+1)

