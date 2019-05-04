module Style
  ( preen
  ) where

import Ansi (bold, italic, resetBold, resetUnderline, underline)

data Style
  = Normal
  | Emphasized Char
  | Code
  | Comment

data Text = Text
  { style :: Style
  , parsed :: String
  , rest :: String
  }

getStyle :: Style -> Char -> (Style, String)
getStyle Comment '—' = (Comment, "—")
getStyle _ '—' = (Comment, "—" ++ italic)
getStyle Normal '*' = (Emphasized '*', bold)
getStyle Normal '_' = (Emphasized '_', bold)
getStyle Normal '`' = (Code, underline)
getStyle (Emphasized '*') '*' = (Normal, resetBold)
getStyle (Emphasized '_') '_' = (Normal, resetBold)
getStyle Code '`' = (Normal, resetUnderline)
getStyle s c = (s, [c])

preen :: String -> String
preen s = parsed $ parseTask $ Text Normal [] s

parseTask :: Text -> Text
parseTask (Text s p (c:t)) = do
  let (s1, c1) = getStyle s c
  parseTask $ Text s1 (p ++ c1) t
parseTask (Text s p ([])) = Text s p []
