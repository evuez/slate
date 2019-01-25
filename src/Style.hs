module Style
  ( preen
  ) where

import Ansi (bold, resetBold, resetUnderline, underline)

data Style
  = Normal
  | Emphasized Char
  | Code
  deriving (Show)

data Text = Text
  { style :: Style
  , parsed :: String
  , rest :: String
  } deriving (Show)

getStyle :: Style -> Char -> (Style, String)
getStyle Normal '*' = (Emphasized '*', bold)
getStyle Normal '_' = (Emphasized '_', bold)
getStyle Normal '`' = (Code, underline)
getStyle (Emphasized '*') '*' = (Normal, resetBold)
getStyle (Emphasized '_') '_' = (Normal, resetBold)
getStyle Code '`' = (Normal, resetUnderline)
getStyle s c = (s, [c])

preen :: String -> String
preen s = parsed $ parseNote $ Text Normal [] s

parseNote :: Text -> Text
parseNote (Text s p (c:t)) = do
  let (s1, c1) = getStyle s c
  parseNote $ Text s1 (p ++ c1) t
parseNote (Text s p ([])) = Text s p []
