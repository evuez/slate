module Lib
  ( initialize
  , execute
  , parser
  ) where

import Data.Semigroup ((<>))
import Options.Applicative
import System.Directory
  ( createDirectoryIfMissing
  , getCurrentDirectory
  , getHomeDirectory
  , removeFile
  , renameFile
  )
import System.FilePath.Posix (takeBaseName)

data Command
  = Add String
        String -- Add a note
  | Check String
          Int -- Check a note
  | Uncheck String
            Int -- Uncheck a note
  | Remove String
           Int -- Remove a note
  | Display String
            String -- Display a slate
  | Rename String
           String -- Rename a slate
  | Wipe String
         String -- Remove all notes in a slate
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

check :: Parser Command
check = Check <$> name <*> argument auto (metavar "NOTE ID")

uncheck :: Parser Command
uncheck = Uncheck <$> name <*> argument auto (metavar "NOTE ID")

remove :: Parser Command
remove = Remove <$> name <*> argument auto (metavar "NOTE ID")

display :: Parser Command
display =
  Display <$> name <*>
  option
    str
    (long "only" <> short 'o' <> help "Display only checked / unchecked notes." <>
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
    (long "only" <> short 'o' <> help "Wipe only checked / unchecked notes." <>
     value "")

parser :: Parser Command
parser =
  subparser
    (command "add" (info add (progDesc "Add a note.")) <>
     command "check" (info check (progDesc "Check a note.")) <>
     command "uncheck" (info uncheck (progDesc "Uncheck a note.")) <>
     command "remove" (info remove (progDesc "Remove a note.")) <>
     command "display" (info display (progDesc "Display a slate.")) <>
     command "rename" (info rename (progDesc "Rename a slate.")) <>
     command "wipe" (info wipe (progDesc "Wipe a slate.")))

-- Commands
execute :: Command -> IO ()
execute (Add "" n) = getSlateName >>= (\s -> execute (Add s n))
execute (Add s n) =
  getSlatePath s >>= (\x -> appendFile x (" - [ ] " ++ n ++ "\n"))
execute (Check "" n) = getSlateName >>= (\x -> execute (Check x n))
execute (Check s n) = getSlatePath s >>= (\x -> checkNote x n)
execute (Uncheck "" n) = getSlateName >>= (\x -> execute (Uncheck x n))
execute (Uncheck s n) = getSlatePath s >>= (\x -> uncheckNote x n)
execute (Remove "" n) = getSlateName >>= (\x -> execute (Remove x n))
execute (Remove s n) = getSlatePath s >>= (\x -> removeNote x n)
execute (Display "" f) = getSlateName >>= (\x -> execute (Display x f))
execute (Display s f) = getSlatePath s >>= (\x -> displaySlate x f)
execute (Rename sc sn) = renameSlate sc sn
execute (Wipe "" f) = getSlateName >>= (\x -> execute (Wipe x f))
execute (Wipe s "") = getSlatePath s >>= removeFile
execute (Wipe s f) = getSlatePath s >>= (\x -> wipeSlate x f)

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

getSlatePath :: String -> IO FilePath
getSlatePath s = do
  dir <- getConfigDirectory
  return $ dir ++ s ++ ".md"

displaySlate :: String -> String -> IO ()
displaySlate s "" = do
  contents <- readFile s
  let notes = zipWith displayNote [0 ..] (lines contents)
  putStr $ unlines notes
displaySlate s "checked" = do
  contents <- readFile s
  let notes = zipWith displayNote [0 ..] (lines contents)
  putStr $ unlines $ filter isNoteChecked notes
displaySlate s "unchecked" = do
  contents <- readFile s
  let notes = zipWith displayNote [0 ..] (lines contents)
  putStr $ unlines $ filter (not . isNoteChecked) notes
displaySlate _ f = putStr $ "\"" ++ f ++ "\" is not a valid filter."

displayNote :: Int -> String -> String
displayNote line (' ':'-':' ':'[':' ':']':note) = padInt line 2 ++ " -" ++ note
displayNote line (' ':'-':' ':'[':'x':']':note) =
  "\x1B[9m" ++ padInt line 2 ++ " -" ++ note ++ "\x1B[0m"
displayNote line _ =
  "\x1B[31m" ++
  padInt line 2 ++ " - Parsing error: line is malformed" ++ "\x1B[0m"

isNoteChecked :: String -> Bool
isNoteChecked (' ':'-':' ':'[':'x':']':_) = True
isNoteChecked ('\x1B':_) = True
isNoteChecked _ = False

padInt :: Int -> Int -> String
padInt n s = replicate (s - length (show n)) '0' ++ show n

checkNote :: FilePath -> Int -> IO ()
checkNote s n = do
  contents <- readFile s
  let (x, y:t) = splitAt n (lines contents)
      c =
        case y of
          ' ':'-':' ':'[':' ':']':note -> " - [x]" ++ note
          note -> note
      tmp = s ++ ".tmp"
  writeFile (s ++ ".tmp") (unlines $ x ++ c : t)
  renameFile tmp s

uncheckNote :: FilePath -> Int -> IO ()
uncheckNote s n = do
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
wipeSlate s "checked" = do
  contents <- readFile s
  let tmp = s ++ ".tmp"
  writeFile tmp $ unlines $ filter (not . isNoteChecked) (lines contents)
  renameFile tmp s
wipeSlate s "unchecked" = do
  contents <- readFile s
  let tmp = s ++ ".tmp"
  writeFile tmp $ unlines $ filter isNoteChecked (lines contents)
  renameFile tmp s
wipeSlate _ f = putStr $ "\"" ++ f ++ "\" is not a valid filter."
