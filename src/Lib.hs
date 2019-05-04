module Lib
  ( initialize
  , execute
  , C.parser
  ) where

import Ansi
  ( Palette(primary, secondary, success, ternary, warning)
  , makeCrossed
  , makeFaint
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
import Task as T (State(..), Task(..), dumpTasks)

execute :: C.Command -> IO ()
execute (C.Add s t) =
  getSlatePath s >>= (\x -> appendFile x (" - [ ] " ++ t ++ "\n"))
execute (C.Done s (Just ti) comment') =
  getSlatePath s >>= (\x -> markAsDone x ti comment')
execute (C.Done s Nothing _) =
  getSlatePath s >>= (\x -> displaySlate x (Just "done"))
execute (C.Todo s (Just ti)) = getSlatePath s >>= (\x -> markAsTodo x ti)
execute (C.Todo s Nothing) =
  getSlatePath s >>= (\x -> displaySlate x (Just "todo"))
execute (C.Doing s (Just ti)) = getSlatePath s >>= (\x -> markAsDoing x ti)
execute (C.Doing s Nothing) =
  getSlatePath s >>= (\x -> displaySlate x (Just "doing"))
execute (C.Edit s) = getSlatePath s >>= editSlate
execute (C.Remove s ti) = getSlatePath s >>= (\x -> removeTask x ti)
execute (C.Display s f) = getSlatePath s >>= (\x -> displaySlate x f)
execute (C.Rename sc sn) = renameSlate sc sn
execute (C.Wipe s Nothing) = getSlatePath s >>= removeFile
execute (C.Wipe s (Just f)) = getSlatePath s >>= (\x -> wipeSlate x f)
execute (C.Status s) = getSlatePath s >>= (\x -> displayStatus x)
execute (C.Sync) = syncSlates

initialize :: IO ()
initialize = configDirectory >>= (\c -> createDirectoryIfMissing True c)

readTasks :: String -> IO [T.Task]
readTasks s = loadTasks 0 [] <$> lines <$> readFile s

loadTasks :: Int -> [T.Task] -> [String] -> [T.Task]
loadTasks l tasks (x:xs) = loadTasks (l + 1) ((buildTask x l) : tasks) xs
loadTasks _ tasks [] = reverse tasks

buildTask :: String -> Int -> T.Task
buildTask (' ':'-':' ':'[':' ':']':' ':'…':' ':task) line' =
  T.Task Doing task Nothing line'
buildTask (' ':'-':' ':'[':' ':']':' ':task) line' =
  T.Task Todo task Nothing line'
buildTask (' ':'-':' ':'[':'x':']':' ':task) line' =
  T.Task Done task Nothing line'
buildTask text' _ = error $ "Error: \"" ++ text' ++ "\" is not a valid task."

displaySlate :: String -> Maybe String -> IO ()
displaySlate s Nothing = putStr =<< unlines <$> displayTasks <$> readTasks s
displaySlate s (Just "done") =
  putStr =<< unlines <$> displayTasks <$> filter F.done <$> readTasks s
displaySlate s (Just "todo") =
  putStr =<< unlines <$> displayTasks <$> filter F.todo <$> readTasks s
displaySlate s (Just "doing") =
  putStr =<< unlines <$> displayTasks <$> filter F.doing <$> readTasks s
displaySlate _ (Just f) = putStrLn $ "\"" ++ f ++ "\" is not a valid filter."

displayTasks :: [T.Task] -> [String]
displayTasks tasks = map (displayTask $ length tasks) tasks

displayBullet :: Int -> Int -> (String -> String) -> String
displayBullet total line' style =
  (alignRight total line') ++ (style $ paint ternary $ show line')

displayTask :: Int -> T.Task -> String
displayTask total (T.Task Todo task Nothing line') =
  ((displayBullet total line' id) ++ " " ++ preen task ++ reset)
displayTask total (T.Task Doing task Nothing line') =
  ((displayBullet total line' makeInverse) ++ " " ++ preen task ++ reset)
displayTask total (T.Task Done task Nothing line') =
  ((displayBullet total line' makeCrossed) ++
   " " ++ (makeFaint $ preen task) ++ reset)

alignRight :: Int -> Int -> String
alignRight x n = replicate (length (show $ x - 1) - length (show n)) ' '

writeTasks :: FilePath -> [Task] -> IO ()
writeTasks s tasks = do
  let tmp = s ++ ".tmp"
  writeFile tmp (dumpTasks tasks)
  renameFile tmp s

markAsDone :: FilePath -> Int -> Maybe String -> IO ()
markAsDone s ti comment' = do
  (x, t:xs) <- splitAt ti <$> (readTasks s)
  writeTasks s $ x ++ (t {T.status = T.Done, T.comment = comment'}) : xs

markAsTodo :: FilePath -> Int -> IO ()
markAsTodo s ti = do
  (x, t:xs) <- splitAt ti <$> (readTasks s)
  writeTasks s $ x ++ (t {T.status = T.Todo}) : xs

markAsDoing :: FilePath -> Int -> IO ()
markAsDoing s ti = writeTasks s . map (markAsDoing' ti) =<< readTasks s

markAsDoing' :: Int -> Task -> Task
markAsDoing' _ t@(Task T.Doing _ _ _) = t {T.status = T.Todo}
markAsDoing' ti t@(Task _ _ _ l)
  | ti == l = t {T.status = T.Doing}
markAsDoing' _ t = t

editSlate :: FilePath -> IO ()
editSlate s = do
  h <- spawnProcess "vim" [s]
  _ <- waitForProcess h
  return ()

removeTask :: FilePath -> Int -> IO ()
removeTask s ti = do
  contents <- readFile s
  let (x, _:t) = splitAt ti (lines contents)
      tmp = s ++ ".tmp"
  writeFile tmp (unlines $ x ++ t)
  renameFile tmp s

renameSlate :: String -> String -> IO ()
renameSlate sc sn = do
  current <- getSlatePath (Just sc)
  new <- getSlatePath (Just sn)
  renameFile current new

wipeSlate :: FilePath -> String -> IO ()
wipeSlate s "done" = writeTasks s . filter F.todo =<< readTasks s
wipeSlate s "todo" = writeTasks s . filter F.done =<< readTasks s
wipeSlate _ f = putStrLn $ "\"" ++ f ++ "\" is not a valid filter."

displayStatus :: FilePath -> IO ()
displayStatus s = do
  (syncColor, syncString) <- getSyncStatus s
  tasks <- readTasks s
  let done = fromIntegral $ length $ filter F.done tasks :: Double
      todo = fromIntegral $ length $ filter F.todo tasks :: Double
      doing =
        fromMaybe "" $
        (\x -> Just $ "\n" ++ x) =<<
        (listToMaybe $ displayTasks $ filter F.doing tasks)
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
