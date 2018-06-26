module AnsiStyle
  ( toAnsi
  ) where

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
getStyle Normal '*' = (Emphasized '*', "\x1B[1m")
getStyle Normal '_' = (Emphasized '_', "\x1B[1m")
getStyle Normal '`' = (Code, "\x1B[4m")
getStyle (Emphasized '*') '*' = (Normal, "\x1B[22m")
getStyle (Emphasized '_') '_' = (Normal, "\x1B[22m")
getStyle Code '`' = (Normal, "\x1B[24m")
getStyle s c = (s, [c])

toAnsi :: String -> String
toAnsi s = parsed $ parseNote $ Text Normal [] s

parseNote :: Text -> Text
parseNote (Text s p (c:t)) = do
  let (s1, c1) = getStyle s c
  parseNote $ Text s1 (p ++ c1) t
parseNote (Text s p ([])) = Text s p []
