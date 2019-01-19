module Command
  ( parser
  , Command(..)
  ) where

import Data.Semigroup ((<>))
import Options.Applicative

type Slate = String

type Note = String

type NoteId = Int

type Filter = String

data Command
  = Add Slate
        Note
  | Done Slate
         (Maybe NoteId)
  | Todo Slate
         (Maybe NoteId)
  | Doing Slate
          (Maybe NoteId)
  | Edit Slate
  | Remove Slate
           NoteId
  | Display Slate
            Filter
  | Rename Slate
           Slate
  | Wipe Slate
         Filter
  | Status Slate
  | Sync
  deriving (Eq, Show)

name :: Parser String
name =
  option
    str
    (long "name" <> short 'n' <> metavar "SLATE" <> help "Name of the slate." <>
     value "")

add :: Parser Command
add = Add <$> name <*> argument str (metavar "NOTE")

done :: Parser Command
done = Done <$> name <*> optional (argument auto (metavar "NOTE ID"))

todo :: Parser Command
todo = Todo <$> name <*> optional (argument auto (metavar "NOTE ID"))

doing :: Parser Command
doing = Doing <$> name <*> optional (argument auto (metavar "NOTE ID"))

edit :: Parser Command
edit = Edit <$> name

remove :: Parser Command
remove = Remove <$> name <*> argument auto (metavar "NOTE ID")

display :: Parser Command
display =
  Display <$> name <*>
  option
    str
    (long "only" <> short 'o' <> help "Display only done / todo notes." <>
     value "")

rename :: Parser Command
rename =
  Rename <$> argument str (metavar "CURRENT" <> help "Current name.") <*>
  argument str (metavar "NEW" <> help "New name.")

wipe :: Parser Command
wipe =
  Wipe <$> name <*>
  option
    str
    (long "only" <> short 'o' <> help "Wipe only done / todo notes." <> value "")

status :: Parser Command
status = Status <$> name

sync :: Command
sync = Sync

parser :: Parser Command
parser =
  subparser
    (command "add" (info add (progDesc "Add a note.")) <>
     command
       "done"
       (info
          done
          (progDesc
             "Mark a note as done when given a note ID, display done notes otherwise.")) <>
     command
       "todo"
       (info
          todo
          (progDesc
             "Mark a note as todo when given a note ID, display todo notes otherwise.")) <>
     command
       "doing"
       (info
          doing
          (progDesc
             "Toggle highlighting on a note when given a note ID, display notes marked as doing otherwise.")) <>
     command "edit" (info edit (progDesc "Open the slate in the default editor.")) <>
     command "remove" (info remove (progDesc "Remove a note.")) <>
     command "display" (info display (progDesc "Display a slate.")) <>
     command "rename" (info rename (progDesc "Rename a slate.")) <>
     command "wipe" (info wipe (progDesc "Wipe a slate.")) <>
     command "status" (info status (progDesc "Display the status of a slate.")) <>
     command "sync" (info (pure sync) (progDesc "Sync every slate.")))
