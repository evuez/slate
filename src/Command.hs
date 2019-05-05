module Command
  ( parser
  , Command(..)
  , Slate
  ) where

import Data.Semigroup ((<>))
import Options.Applicative

type Slate = String

type Task = String

type TaskId = Int

type Filter = String

type Comment = String

data Command
  = Add (Maybe Slate)
        Task
  | Display (Maybe Slate)
            (Maybe Filter)
  | Doing (Maybe Slate)
          (Maybe TaskId)
  | Done (Maybe Slate)
         (Maybe TaskId)
         (Maybe Comment)
  | Edit (Maybe Slate)
  | Remove (Maybe Slate)
           TaskId
  | Rename Slate
           Slate
  | Status (Maybe Slate)
  | Sync
  | Todo (Maybe Slate)
         (Maybe TaskId)
  | Wipe (Maybe Slate)
         (Maybe Filter)
  deriving (Eq, Show)

name :: Parser (Maybe Slate)
name =
  optional
    (option
       str
       (long "name" <> short 'n' <> metavar "SLATE" <> help "Name of the slate."))

taskId :: Parser TaskId
taskId = argument auto (metavar "TASK ID")

add :: Parser Command
add = Add <$> name <*> argument str (metavar "TASK")

done :: Parser Command
done =
  Done <$> name <*> optional taskId <*>
  optional
    (option
       str
       (long "comment" <> short 'c' <> metavar "COMMENT" <>
        help "Additional comment."))

todo :: Parser Command
todo = Todo <$> name <*> optional taskId

doing :: Parser Command
doing = Doing <$> name <*> optional taskId

edit :: Parser Command
edit = Edit <$> name

remove :: Parser Command
remove = Remove <$> name <*> taskId

display :: Parser Command
display =
  Display <$> name <*>
  optional
    (option
       str
       (long "only" <> short 'o' <> help "Display only done / todo tasks."))

rename :: Parser Command
rename =
  Rename <$> argument str (metavar "CURRENT" <> help "Current name.") <*>
  argument str (metavar "NEW" <> help "New name.")

wipe :: Parser Command
wipe =
  Wipe <$> name <*>
  optional
    (option
       str
       (long "only" <> short 'o' <> help "Wipe only done / todo tasks."))

status :: Parser Command
status = Status <$> name

sync :: Command
sync = Sync

parser :: Parser Command
parser =
  subparser
    (command "add" (info add (progDesc "Add a task.")) <>
     command
       "done"
       (info
          done
          (progDesc
             "Mark a task as done when given a task ID, display done tasks otherwise.")) <>
     command
       "todo"
       (info
          todo
          (progDesc
             "Mark a task as todo when given a task ID, display todo tasks otherwise.")) <>
     command
       "doing"
       (info
          doing
          (progDesc
             "Toggle highlighting on a task when given a task ID, display tasks marked as doing otherwise.")) <>
     command
       "edit"
       (info edit (progDesc "Open the slate in the default editor.")) <>
     command "remove" (info remove (progDesc "Remove a task.")) <>
     command "display" (info display (progDesc "Display a slate.")) <>
     command "rename" (info rename (progDesc "Rename a slate.")) <>
     command "wipe" (info wipe (progDesc "Wipe a slate.")) <>
     command "status" (info status (progDesc "Display the status of a slate.")) <>
     command "sync" (info (pure sync) (progDesc "Sync every slate.")))
