module Main where

import Lib (execute, initialize, parser)
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
        (fullDesc <> progDesc "Slate" <> header "slate - a task taking tool.")
