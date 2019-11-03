{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Config
  ( getConfigValue
  , configDirectory
  , getSlatePath
  , config
  ) where

import qualified Data.HashMap.Lazy as M (lookup)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.String.Conversions (convertString)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath.Posix (takeBaseName)
import Text.Toml (parseTomlDoc)
import Text.Toml.Types (Node(VString), Node(VTable), Table)

class GetConfig a where
  getConfigValue :: (String, String) -> IO a

instance GetConfig (Maybe String) where
  getConfigValue (s, k) = do
    c <- config
    return $
      case (M.lookup (fromString s) c) of
        Just (VTable t) ->
          case (M.lookup (fromString k) t) of
            Just (VString v) -> Just (convertString v)
            _ -> Nothing
        _ -> Nothing

instance GetConfig String where
  getConfigValue (s, k) = do
    f <- configFile
    c <- getConfigValue (s, k)
    return $ maybe (error $ "Key `" ++ k ++ "` not found in " ++ f ++ ".") id c

config :: IO Table
config =
  (\(Right x) -> x) <$>
  (parseTomlDoc "" <$> fromString <$> (readFile =<< configFile))

configDirectory :: IO String
configDirectory = do
  home <- getHomeDirectory
  return $ home ++ "/.config/slate/"

configFile :: IO String
configFile = do
  dir <- configDirectory
  return $ dir ++ "config.toml"

slateName :: IO String
slateName = do
  d <- getCurrentDirectory
  let headOrFail =
        \x ->
          maybe
            (error
               "Found a .slate file in the current directory but it is empty.")
            id
            (listToMaybe x)
  doesFileExist (d ++ "/.slate") >>= \case
    True -> readFile (d ++ "/.slate") >>= (return . headOrFail . lines)
    False -> return $ takeBaseName d

getSlatePath :: Maybe String -> IO FilePath
getSlatePath Nothing = do
  s <- slateName
  dir <- configDirectory
  return $ dir ++ s ++ ".md"
getSlatePath (Just s) = do
  dir <- configDirectory
  return $ dir ++ s ++ ".md"
