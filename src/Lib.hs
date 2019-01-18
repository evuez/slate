module Lib
  ( initialize
  , execute
  , parser
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
import Command (Command(..), parser)
import Config (configDirectory, getConfigValue, getSlatePath)
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
  , std_out
  , waitForProcess
  )

execute :: Command -> IO ()
execute (Add s n) =
  getSlatePath s >>= (\x -> appendFile x (" - [ ] " ++ n ++ "\n"))
execute (Done s (Just n)) = getSlatePath s >>= (\x -> markAsDone x n)
execute (Done s Nothing) = getSlatePath s >>= (\x -> displaySlate x "done")
execute (Todo s (Just n)) = getSlatePath s >>= (\x -> markAsTodo x n)
execute (Todo s Nothing) = getSlatePath s >>= (\x -> displaySlate x "todo")
execute (Doing s (Just n)) = getSlatePath s >>= (\x -> markAsDoing x n)
execute (Doing s Nothing) = getSlatePath s >>= (\x -> displaySlate x "doing")
execute (Remove s n) = getSlatePath s >>= (\x -> removeNote x n)
execute (Display s f) = getSlatePath s >>= (\x -> displaySlate x f)
execute (Rename sc sn) = renameSlate sc sn
execute (Wipe s "") = getSlatePath s >>= removeFile
execute (Wipe s f) = getSlatePath s >>= (\x -> wipeSlate x f)
execute (Status s) = getSlatePath s >>= (\x -> displayStatus x)
execute (Sync) = syncSlates

initialize :: IO ()
initialize = configDirectory >>= (\c -> createDirectoryIfMissing True c)

displaySlate :: String -> String -> IO ()
displaySlate s "" = do
  contents <- readFile s
  putStr $ unlines $ displayNotes (lines contents)
displaySlate s "done" = do
  contents <- readFile s
  putStr $ unlines $ filter F.done $ displayNotes (lines contents)
displaySlate s "todo" = do
  contents <- readFile s
  putStr $ unlines $ filter F.todo $ displayNotes (lines contents)
displaySlate s "doing" = do
  contents <- readFile s
  putStr $ unlines $ filter F.doing $ displayNotes (lines contents)
displaySlate _ f = putStrLn $ "\"" ++ f ++ "\" is not a valid filter."

displayNotes :: [String] -> [String]
displayNotes notes = zipWith (displayNote $ length notes) [0 ..] notes

displayNote :: Int -> Int -> String -> String
displayNote total line (' ':'-':' ':'[':' ':']':' ':'>':note) =
  makeInverse $ (paint ternary $ alignRight total line) ++ " -" ++ preen note ++ reset
displayNote total line (' ':'-':' ':'[':' ':']':note) =
  (paint ternary $ alignRight total line) ++ " -" ++ preen note ++ reset
displayNote total line (' ':'-':' ':'[':'x':']':note) =
  makeCrossed $ (paint ternary $ alignRight total line) ++ " -" ++ preen note ++ reset
displayNote total line _ =
  (paint warning) $
  (paint ternary $ alignRight total line) ++
  " - Parsing error: line is malformed"

alignRight :: Int -> Int -> String
alignRight x n = replicate (length (show x) - length (show n)) ' ' ++ show n

markAsDone :: FilePath -> Int -> IO ()
markAsDone s n = do
  contents <- readFile s
  let (x, y:t) = splitAt n (lines contents)
      c =
        case y of
          ' ':'-':' ':'[':' ':']':' ':'>':note -> " - [x]" ++ note
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

markAsDoing :: FilePath -> Int -> IO ()
markAsDoing s n = do
  contents <- readFile s
  let ls = zipWith (removeDoingMarkForOthers n) [0 ..] (lines contents)
  let (x, y:t) = splitAt n ls
      c =
        case y of
          ' ':'-':' ':'[':m:']':' ':'>':note -> " - [" ++ [m] ++ "]" ++ note
          ' ':'-':' ':'[':_:']':note -> " - [ ] >" ++ note
          note -> note
      tmp = s ++ ".tmp"
  writeFile tmp (unlines $ x ++ c : t)
  renameFile tmp s

removeDoingMarkForOthers :: Int -> Int -> String -> String
removeDoingMarkForOthers k l p@(' ':'-':' ':'[':m:']':' ':'>':n)
  | k /= l = " - [" ++ [m] ++ "]" ++ n
  | otherwise = p
removeDoingMarkForOthers _ _ n = n

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
  let done = fromIntegral $ length $ filter F.done (lines contents) :: Double
      todo = fromIntegral $ length $ filter F.todo (lines contents) :: Double
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
  putStrLn $ mconcat [mconcat stats, "\n", progress percent statsLength]

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
