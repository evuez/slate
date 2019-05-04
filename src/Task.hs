module Task
  ( Task(..)
  , State(..)
  , dumpTasks
  ) where

data State
  = Todo
  | Doing
  | Done

data Task = Task
  { status :: State
  , text :: String
  , comment :: Maybe String
  , line :: Int
  }

dumpTask :: Task -> String
dumpTask (Task state' text' comment' _) =
  (mconcat [" - ", dumpState state', text', dumpComment comment'])

dumpState :: State -> String
dumpState Todo = "[ ] "
dumpState Doing = "[ ] … "
dumpState Done = "[x] "

dumpComment :: Maybe String -> String
dumpComment (Just comment') = " — " ++ comment'
dumpComment Nothing = ""

dumpTasks :: [Task] -> String
dumpTasks tasks = unlines $ map dumpTask tasks
