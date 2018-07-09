module Ansi where

progress :: Double -> Int -> String
progress percent size =
  makeInverse
    ((replicate sizeCompleted ' ') ++ makeGrey (replicate sizeRemaining ' '))
  where
    size' = fromIntegral size :: Double
    sizeCompleted = round $ size' * percent / 100
    sizeRemaining = size - sizeCompleted

reset :: String
reset = "\x1B[0m"

bold :: String
bold = "\x1B[1m"

underline :: String
underline = "\x1B[4m"

inverse :: String
inverse = "\x1B[7m"

crossed :: String
crossed = "\x1B[9m"

resetEmphasis :: String
resetEmphasis = "\x1B[22m"

resetUnderline :: String
resetUnderline = "\x1B[24m"

red :: String
red = "\x1B[31m"

green :: String
green = "\x1B[32m"

grey :: String
grey = "\x1B[2;37m"

makeBold :: String -> String
makeBold s = bold ++ s ++ reset

makeUnderline :: String -> String
makeUnderline s = underline ++ s ++ reset

makeInverse :: String -> String
makeInverse s = inverse ++ s ++ reset

makeCrossed :: String -> String
makeCrossed s = crossed ++ s ++ reset

makeResetEmphasis :: String -> String
makeResetEmphasis s = resetEmphasis ++ s ++ reset

makeResetUnderline :: String -> String
makeResetUnderline s = resetUnderline ++ s ++ reset

makeRed :: String -> String
makeRed s = red ++ s ++ reset

makeGreen :: String -> String
makeGreen s = green ++ s ++ reset

makeGrey :: String -> String
makeGrey s = grey ++ s ++ reset
