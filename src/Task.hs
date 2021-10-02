module Task
  ( Task(..)
  , Status(..)
  , dumpTasks
  , showTasks
  , loadTasks
  ) where

import Ansi
  ( Palette(ternary)
  , makeCrossed
  , makeFaint
  , makeInverse
  , makeItalic
  , paint
  , reset
  )
import Style (preen)

data Status
  = Todo
  | Doing
  | Done

data Task = Task
  { line :: Int
  , status :: Status
  , text :: String
  , comment :: Maybe String
  }

-- Dump
dumpTasks :: [Task] -> String
dumpTasks tasks = unlines $ map dumpTask tasks

dumpTask :: Task -> String
dumpTask (Task _ status' text' comment') =
  mconcat [" - ", dumpStatus status', text', dumpComment comment']

dumpStatus :: Status -> String
dumpStatus Todo = "[ ] "
dumpStatus Doing = "[ ] … "
dumpStatus Done = "[x] "

dumpComment :: Maybe String -> String
dumpComment (Just comment') = " — " ++ comment'
dumpComment Nothing = ""

-- Load
loadTasks :: Int -> [Task] -> [String] -> [Task]
loadTasks l tasks (x:xs) = loadTasks (l + 1) (buildTask x l : tasks) xs
loadTasks _ tasks [] = reverse tasks

buildTask :: String -> Int -> Task
buildTask (' ':'-':' ':'[':' ':']':' ':'…':' ':r) line' =
  buildTask' (Task line' Doing) r
buildTask (' ':'-':' ':'[':' ':']':' ':r) line' = buildTask' (Task line' Todo) r
buildTask (' ':'-':' ':'[':'x':']':' ':r) line' = buildTask' (Task line' Done) r
buildTask text' _ = error $ "Error: \"" ++ text' ++ "\" is not a valid task."

buildTask' :: (String -> Maybe String -> Task) -> String -> Task
buildTask' partialTask text' =
  uncurry partialTask $ splitComment text' ("", Nothing)

splitComment :: String -> (String, Maybe String) -> (String, Maybe String)
splitComment [] (t, c) = (reverse t, c)
splitComment (' ':'—':' ':xs) (t, Nothing) = (reverse t, Just xs)
splitComment (x:xs) (t, _) = splitComment xs (x : t, Nothing)

-- Show
showTasks :: [Task] -> [String]
showTasks tasks = map (showTask $ length tasks) tasks

showTask :: Int -> Task -> String
showTask total (Task line' Todo text' comment') =
  showLine total line' id ++ " " ++ showText text' comment' id ++ reset
showTask total (Task line' Doing text' comment') =
  showLine total line' makeInverse ++ " " ++ showText text' comment' id ++ reset
showTask total (Task line' Done text' comment') =
  showLine total line' makeCrossed ++
  " " ++ showText text' comment' makeFaint ++ reset

showLine :: Int -> Int -> (String -> String) -> String
showLine total line' style =
  alignRight total line' ++ style (paint ternary $ show line')
  where
    alignRight x n = replicate (length (show $ x - 1) - length (show n)) ' '

showText :: String -> Maybe String -> (String -> String) -> String
showText text' (Just comment') style =
  style $ preen text' ++ makeItalic (" — " ++ comment')
showText text' Nothing style = style $ preen text'
