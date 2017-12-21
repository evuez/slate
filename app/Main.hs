module Main where

import Lib
import Options.Applicative
import Data.Semigroup ((<>))

main :: IO ()
main = do
  initialize
  parsedOpts <- execParser opts
  execute parsedOpts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Slate"
     <> header "slate - a note taking tool." )
