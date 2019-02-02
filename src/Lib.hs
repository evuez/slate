module Lib
  ( initialize
  , execute
  , C.parser
  ) where

import Ansi
  ( Palette(primary, secondary, success, ternary, warning)
  , makeCrossed
  , makeInverse
  , paint
  , palette
  , progress
  , reset
  )
import qualified Command as C (Command(..), parser)
import Config (configDirectory, getConfigValue, getSlatePath)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Filter as F (doing, done, todo)
import Style (preen)
import System.Directory (createDirectoryIfMissing, removeFile, renameFile)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process
  ( StdStream(NoStream)
  , createProcess
  , cwd
  , env
  , shell
  , spawnProcess
  , std_out
  , waitForProcess
  )

data State
  = Todo
  | Doing
  | Done

data Task = Task -- Add line number!!
  { status :: State
  , text :: String
  , comment :: Maybe String
  , children :: [Task]
  }

dumpTask :: Task -> String
dumpTask (Task state text comment children) = (mconcat [" - ", dumpState state, text, dumpComment comment, concatMap (("\n  " ++) . dumpTask) children])

dumpState :: State -> String
dumpState Todo = "[ ] "
dumpState Doing = "[ ] …"
dumpState Done = "[x] "

dumpComment :: Maybe String -> String
dumpComment (Just comment) = " — " ++ comment
dumpComment Nothing = ""

dumpChildren :: [Task] -> [String]
dumpChildren tasks = map (("  " ++) . dumpTask) tasks

execute :: C.Command -> IO ()
execute (C.Add s Nothing n) =
  getSlatePath s >>= (\x -> appendFile x (" - [ ] " ++ n ++ "\n"))
execute (C.Add s (Just p) n) = getSlatePath s >>= (\x -> addSubNote x p n)
execute (C.Done s (Just n) comment) =
  getSlatePath s >>= (\x -> markAsDone x n comment)
execute (C.Done s Nothing _) =
  getSlatePath s >>= (\x -> displaySlate x (Just "done"))
execute (C.Todo s (Just n)) = getSlatePath s >>= (\x -> markAsTodo x n)
execute (C.Todo s Nothing) =
  getSlatePath s >>= (\x -> displaySlate x (Just "todo"))
execute (C.Doing s (Just n)) = getSlatePath s >>= (\x -> markAsDoing x n)
execute (C.Doing s Nothing) =
  getSlatePath s >>= (\x -> displaySlate x (Just "doing"))
execute (C.Edit s) = getSlatePath s >>= editSlate
execute (C.Remove s n) = getSlatePath s >>= (\x -> removeNote x n)
execute (C.Display s f) = getSlatePath s >>= (\x -> displaySlate x f)
execute (C.Rename sc sn) = renameSlate sc sn
execute (C.Wipe s Nothing) = getSlatePath s >>= removeFile
execute (C.Wipe s (Just f)) = getSlatePath s >>= (\x -> wipeSlate x f)
execute (C.Status s) = getSlatePath s >>= (\x -> displayStatus x)
execute (C.Sync) = syncSlates

initialize :: IO ()
initialize = configDirectory >>= (\c -> createDirectoryIfMissing True c)

addSubNote :: String -> Int -> String -> IO ()
addSubNote s p n = do
  notes <- readNotes s
  let (head, parent:rest) = splitAt p notes
      updatedParent = addChild parent (Task Todo n Nothing [])
      tmp = s ++ ".tmp"
  writeFile
    (s ++ ".tmp")
    (dumpTasks $ head ++ (updatedParent:rest))
  renameFile tmp s

isSubNote :: String -> Bool
isSubNote (' ':' ':' ':'-':_) = True
isSubNote _ = False

readNotes :: String -> IO [Task]
--readNotes s = map buildNote <$> lines <$> readFile s
readNotes s = loadNotes [] <$> lines <$> readFile s

loadNotes :: [Task] -> [String] -> [Task]
loadNotes (parent:rest) (child@(' ':' ':'-':_):xs) = loadNotes ((addChild parent (buildNote child)):rest) xs
loadNotes [] (x:xs) = loadNotes [buildNote x] xs
loadNotes tasks (x:xs) = loadNotes (( buildNote x ):tasks) xs
loadNotes tasks [] = reverse tasks

addChild :: Task -> Task -> Task
addChild parent child  = parent { children = (children parent) ++ [child] }

buildNote :: String -> Task
buildNote (' ':'-':' ':'[':' ':']':' ':'…':' ':note) = Task Doing note Nothing []
buildNote (' ':'-':' ':'[':' ':']':' ':note) = Task Todo note Nothing []
buildNote (' ':'-':' ':'[':'x':']':' ':note) = Task Done note Nothing []
buildNote (' ':xs@(' ':'-':_)) = buildNote xs
buildNote text = error $ "Error: \"" ++ text ++ "\" is not a valid task."

dumpTasks :: [Task] -> String
dumpTasks tasks = unlines $ map dumpTask tasks

displaySlate :: String -> Maybe String -> IO ()
displaySlate s Nothing = putStr =<< unlines <$> displayNotes <$> readNotes s
displaySlate s (Just "done") =
  putStr =<< unlines <$> filter F.done <$> displayNotes <$> readNotes s
displaySlate s (Just "todo") =
  putStr =<< unlines <$> filter F.todo <$> displayNotes <$> readNotes s
displaySlate s (Just "doing") =
  putStr =<< unlines <$> filter F.doing <$> displayNotes <$> readNotes s
displaySlate _ (Just f) = putStrLn $ "\"" ++ f ++ "\" is not a valid filter."

displayNotes :: [Task] -> [String]
displayNotes notes = zipWith (displayNote $ length notes) [0 ..] notes

displayNote :: Int -> Int -> Task -> String
displayNote total line (Task Doing note Nothing []) =
  makeInverse $
  (paint ternary $ alignRight total line) ++ " " ++ preen note ++ reset
displayNote total line (Task Todo note Nothing []) =
  (paint ternary $ alignRight total line) ++ " " ++ preen note ++ reset
displayNote total line (Task Done note Nothing []) =
  makeCrossed $
  (paint ternary $ alignRight total line) ++ " " ++ preen note ++ reset

alignRight :: Int -> Int -> String
alignRight x n =
  replicate (length (show $ x - 1) - length (show n)) ' ' ++ show n

markAsDone :: FilePath -> Int -> Maybe String -> IO ()
markAsDone s n comment = do
  contents <- readFile s
  let (x, y:t) = splitAt n (lines contents)
      comment' = fromMaybe "" $ comment >>= (\c' -> Just $ " — " ++ c')
      c =
        case y of
          ' ':'-':' ':'[':' ':']':' ':'…':note -> " - [x]" ++ note ++ comment'
          ' ':'-':' ':'[':' ':']':note -> " - [x]" ++ note ++ comment'
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

markAsDoing :: FilePath -> Int -> IO ()
markAsDoing s n = do
  contents <- readFile s
  let ls = zipWith (removeDoingMarkForOthers n) [0 ..] (lines contents)
  let (x, y:t) = splitAt n ls
      c =
        case y of
          ' ':'-':' ':'[':m:']':' ':'…':note -> " - [" ++ [m] ++ "]" ++ note
          ' ':'-':' ':'[':_:']':note -> " - [ ] …" ++ note
          note -> note
      tmp = s ++ ".tmp"
  writeFile tmp (unlines $ x ++ c : t)
  renameFile tmp s

removeDoingMarkForOthers :: Int -> Int -> String -> String
removeDoingMarkForOthers k l p@(' ':'-':' ':'[':m:']':' ':'…':n)
  | k /= l = " - [" ++ [m] ++ "]" ++ n
  | otherwise = p
removeDoingMarkForOthers _ _ n = n

editSlate :: FilePath -> IO ()
editSlate s = do
  h <- spawnProcess "vim" [s]
  _ <- waitForProcess h
  return ()

removeNote :: FilePath -> Int -> IO ()
removeNote s n = do
  contents <- readFile s
  let (x, _:t) = splitAt n (lines contents)
      tmp = s ++ ".tmp"
  writeFile tmp (unlines $ x ++ t)
  renameFile tmp s

renameSlate :: String -> String -> IO ()
renameSlate sc sn = do
  current <- getSlatePath (Just sc)
  new <- getSlatePath (Just sn)
  renameFile current new

wipeSlate :: FilePath -> String -> IO ()
wipeSlate s "done" = do
  contents <- readFile s
  let tmp = s ++ ".tmp"
  writeFile tmp $ unlines $ filter F.todo (lines contents)
  renameFile tmp s
wipeSlate s "todo" = do
  contents <- readFile s
  let tmp = s ++ ".tmp"
  writeFile tmp $ unlines $ filter F.done (lines contents)
  renameFile tmp s
wipeSlate _ f = putStrLn $ "\"" ++ f ++ "\" is not a valid filter."

displayStatus :: FilePath -> IO ()
displayStatus s = do
  (syncColor, syncString) <- getSyncStatus s
  contents <- readFile s
  notes <- readNotes s
  let done = fromIntegral $ length $ filter F.done (lines contents) :: Double -- check if we still need string-based filters
      todo = fromIntegral $ length $ filter F.todo (lines contents) :: Double
      doing =
        fromMaybe "" $
        (\x -> Just $ "\n" ++ x) =<<
        (listToMaybe $ filter F.doing $ displayNotes notes)
      percent = done / (done + todo) * 100
      stats =
        [ (ternary palette)
        , show (round percent :: Integer)
        , "% · "
        , reset
        , (primary palette)
        , show (round done :: Integer)
        , reset
        , " done · "
        , (secondary palette)
        , show (round todo :: Integer)
        , reset
        , " todo — "
        , syncColor
        , syncString
        , reset
        ]
      statsLength = sum [length (x : xs) | x:xs <- stats, x /= '\ESC']
  putStrLn $ mconcat [mconcat stats, "\n", progress percent statsLength, doing]

getSyncStatus :: FilePath -> IO (String, String)
getSyncStatus s = do
  v <- getConfigValue ("callbacks", "status")
  case v of
    (Just c) -> do
      d <- configDirectory
      (_, _, _, h) <-
        createProcess
          (shell c)
          {cwd = Just d, std_out = NoStream, env = Just [("SLATE", s)]}
      e <- waitForProcess h
      return $
        case e of
          ExitSuccess -> ((success palette), "sync ✔")
          (ExitFailure _) -> ((warning palette), "sync ✘")
    Nothing -> return ("", "")

syncSlates :: IO ()
syncSlates = do
  c <- getConfigValue ("callbacks", "sync")
  d <- configDirectory
  (_, _, _, h) <- createProcess (shell c) {cwd = Just d}
  _ <- waitForProcess h
  putStrLn $ paint success "Done syncing ✔"
