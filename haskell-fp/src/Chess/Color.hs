module Chess.Color (
   Color (Black, White),
   other) where

-- class CellColor 

data Color = Black | White
   deriving (Eq)

other :: Color -> Color
other White = Black
other Black = White

instance Show Color where
  -- https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences
  -- Reset \ESC[m\STX
  show Black = "\ESC[40m\STX  \ESC[m\STX"
  -- show Cyan  = "\ESC[46m\STX  \ESC[m\STX"
  show White = "\ESC[47m\STX  \ESC[m\STX"
