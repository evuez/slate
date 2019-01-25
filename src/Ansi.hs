module Ansi where

progress :: Double -> Int -> String
progress percent size =
  makeInverse
    ((primary palette) ++
     (replicate sizeCompleted ' ') ++
     (secondary palette) ++ (replicate sizeRemaining ' '))
  where
    size' = fromIntegral size :: Double
    sizeCompleted = round $ size' * percent / 100
    sizeRemaining = size - sizeCompleted

reset :: String
reset = "\x1B[0m"

bold :: String
bold = "\x1B[1m"

italic :: String
italic = "\x1B[3m"

underline :: String
underline = "\x1B[4m"

inverse :: String
inverse = "\x1B[7m"

crossed :: String
crossed = "\x1B[9m"

resetBold :: String
resetBold = "\x1B[22m"

resetItalic :: String
resetItalic = "\x1B[23m"

resetUnderline :: String
resetUnderline = "\x1B[24m"

red :: String
red = "\x1B[31m"

green :: String
green = "\x1B[32m"

grey :: String
grey = "\x1B[2;37m"

data Palette = Palette
  { primary :: String
  , secondary :: String
  , ternary :: String
  , success :: String
  , warning :: String
  } deriving (Show)

palette :: Palette
palette =
  Palette
    "\x1B[38;5;97m"
    "\x1B[38;5;74m"
    "\x1B[38;5;178m"
    "\x1B[38;5;72m"
    "\x1B[38;5;167m"

makeBold :: String -> String
makeBold s = bold ++ s ++ reset

makeUnderline :: String -> String
makeUnderline s = underline ++ s ++ reset

makeInverse :: String -> String
makeInverse s = inverse ++ s ++ reset

makeCrossed :: String -> String
makeCrossed s = crossed ++ s ++ reset

paint :: (Palette -> String) -> String -> String
paint f s = (f palette) ++ s ++ reset
