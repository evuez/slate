module Lib
  ( initialize
  , execute
  , parser
  ) where

import AnsiStyle (toAnsi)
import qualified Data.HashMap.Lazy as M (lookup)
import Data.Semigroup ((<>))
import Data.String (fromString)
import Data.String.Conversions (convertString)
import Options.Applicative
import System.Directory
  ( createDirectoryIfMissing
  , getCurrentDirectory
  , getHomeDirectory
  , removeFile
  , renameFile
  )
import System.FilePath.Posix (takeBaseName)
import System.Process (createProcess, cwd, shell, waitForProcess)
import Text.Toml (parseTomlDoc)
import Text.Toml.Types (Node(VString))

type Slate = String

type Note = String

type NoteId = Int

type MaybeNoteId = Maybe Int

type Filter = String

data Command
  = Add Slate
        Note
  | Done Slate
         MaybeNoteId
  | Todo Slate
         MaybeNoteId
  | Remove Slate
           NoteId
  | Display Slate
            Filter
  | Rename Slate
           Slate
  | Wipe Slate
         Filter
  | Sync
  deriving (Eq, Show)

-- Parsers
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
     command "remove" (info remove (progDesc "Remove a note.")) <>
     command "display" (info display (progDesc "Display a slate.")) <>
     command "rename" (info rename (progDesc "Rename a slate.")) <>
     command "wipe" (info wipe (progDesc "Wipe a slate.")) <>
     command "sync" (info (pure sync) (progDesc "Sync every slate.")))

-- Commands
execute :: Command -> IO ()
execute (Add "" n) = getSlateName >>= (\s -> execute (Add s n))
execute (Add s n) =
  getSlatePath s >>= (\x -> appendFile x (" - [ ] " ++ n ++ "\n"))
execute (Done "" n) = getSlateName >>= (\x -> execute (Done x n))
execute (Done s (Just n)) = getSlatePath s >>= (\x -> markAsDone x n)
execute (Done s Nothing) = getSlatePath s >>= (\x -> displaySlate x "done")
execute (Todo "" n) = getSlateName >>= (\x -> execute (Todo x n))
execute (Todo s (Just n)) = getSlatePath s >>= (\x -> markAsTodo x n)
execute (Todo s Nothing) = getSlatePath s >>= (\x -> displaySlate x "todo")
execute (Remove "" n) = getSlateName >>= (\x -> execute (Remove x n))
execute (Remove s n) = getSlatePath s >>= (\x -> removeNote x n)
execute (Display "" f) = getSlateName >>= (\x -> execute (Display x f))
execute (Display s f) = getSlatePath s >>= (\x -> displaySlate x f)
execute (Rename sc sn) = renameSlate sc sn
execute (Wipe "" f) = getSlateName >>= (\x -> execute (Wipe x f))
execute (Wipe s "") = getSlatePath s >>= removeFile
execute (Wipe s f) = getSlatePath s >>= (\x -> wipeSlate x f)
execute (Sync) = syncSlates

-- Helpers
initialize :: IO ()
initialize = getConfigDirectory >>= (\c -> createDirectoryIfMissing True c)

getSlateName :: IO String
getSlateName = do
  directory <- getCurrentDirectory
  return $ takeBaseName directory

getConfigDirectory :: IO String
getConfigDirectory = do
  home <- getHomeDirectory
  return $ home ++ "/.config/slate/"

getConfigFile :: IO String
getConfigFile = do
  dir <- getConfigDirectory
  return $ dir ++ "config.toml"

getSlatePath :: String -> IO FilePath
getSlatePath s = do
  dir <- getConfigDirectory
  return $ dir ++ s ++ ".md"

displaySlate :: String -> String -> IO ()
displaySlate s "" = do
  contents <- readFile s
  let notes = zipWith displayNote [0 ..] (lines contents)
  putStr $ unlines notes
displaySlate s "done" = do
  contents <- readFile s
  let notes = zipWith displayNote [0 ..] (lines contents)
  putStr $ unlines $ filter isNoteDone notes
displaySlate s "todo" = do
  contents <- readFile s
  let notes = zipWith displayNote [0 ..] (lines contents)
  putStr $ unlines $ filter (not . isNoteDone) notes
displaySlate _ f = putStr $ "\"" ++ f ++ "\" is not a valid filter."

displayNote :: Int -> String -> String
displayNote line (' ':'-':' ':'[':' ':']':note) =
  padInt line 2 ++ " -" ++ (toAnsi note)
displayNote line (' ':'-':' ':'[':'x':']':note) =
  "\x1B[9m" ++ padInt line 2 ++ " -" ++ (toAnsi note) ++ "\x1B[0m"
displayNote line _ =
  "\x1B[31m" ++
  padInt line 2 ++ " - Parsing error: line is malformed" ++ "\x1B[0m"

isNoteDone :: String -> Bool
isNoteDone (' ':'-':' ':'[':'x':']':_) = True
isNoteDone ('\x1B':_) = True
isNoteDone _ = False

padInt :: Int -> Int -> String
padInt n s = replicate (s - length (show n)) '0' ++ show n

markAsDone :: FilePath -> Int -> IO ()
markAsDone s n = do
  contents <- readFile s
  let (x, y:t) = splitAt n (lines contents)
      c =
        case y of
          ' ':'-':' ':'[':' ':']':note -> " - [x]" ++ note
          note -> note
      tmp = s ++ ".tmp"
  writeFile (s ++ ".tmp") (unlines $ x ++ c : t)
  renameFile tmp s

markAsTodo :: FilePath -> Int -> IO ()
markAsTodo s n = do
  contents <- readFile s
  let (x, y:t) = splitAt n (lines contents)
      c =
        case y of
          ' ':'-':' ':'[':'x':']':note -> " - [ ]" ++ note
          note -> note
      tmp = s ++ ".tmp"
  writeFile tmp (unlines $ x ++ c : t)
  renameFile tmp s

removeNote :: FilePath -> Int -> IO ()
removeNote s n = do
  contents <- readFile s
  let (x, _:t) = splitAt n (lines contents)
      tmp = s ++ ".tmp"
  writeFile tmp (unlines $ x ++ t)
  renameFile tmp s

renameSlate :: String -> String -> IO ()
renameSlate sc sn = do
  current <- getSlatePath sc
  new <- getSlatePath sn
  renameFile current new

wipeSlate :: FilePath -> String -> IO ()
wipeSlate s "done" = do
  contents <- readFile s
  let tmp = s ++ ".tmp"
  writeFile tmp $ unlines $ filter (not . isNoteDone) (lines contents)
  renameFile tmp s
wipeSlate s "todo" = do
  contents <- readFile s
  let tmp = s ++ ".tmp"
  writeFile tmp $ unlines $ filter isNoteDone (lines contents)
  renameFile tmp s
wipeSlate _ f = putStr $ "\"" ++ f ++ "\" is not a valid filter."

syncSlates :: IO ()
syncSlates = do
  config <- (getConfigFile >>= readFile)
  dir <- getConfigDirectory
  let Right c = parseTomlDoc "" (fromString config)
  let Just (VString s) = M.lookup (fromString "sync") c
  (_, _, _, h) <-
    createProcess (shell (fromString $ convertString s)) {cwd = Just dir}
  _ <- waitForProcess h
  return ()
