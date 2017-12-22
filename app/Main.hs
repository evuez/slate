module Main where

import Data.Semigroup ((<>))
import Lib
import Options.Applicative

main :: IO ()
main = do
  initialize
  parsedOpts <- execParser opts
  execute parsedOpts
  where
    opts =
      info
        (parser <**> helper)
        (fullDesc <> progDesc "Slate" <> header "slate - a note taking tool.")
