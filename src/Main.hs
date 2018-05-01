module Main where

import           Data.List
import           Data.Ini (lookupValue, sections, keys, readIniFile)
import           System.Environment
import           Ast
import           Data.Text (unpack)
import System.Exit
import           Data.String.Utils


exit    = exitWith ExitSuccess

main = do
     args <- getArgs
     case args of
        ["-h"] -> putStr "Subs - more than snippets, less than a transpiler" >> exit
        ["-v"] -> putStr "Subs - Version 0.1" >> exit
        ["-k"] -> showKeys >> exit
        otherwise -> run

showKeys = do
  ini <- parseConfig
  let iniSections = sections ini
      keysText = map (\x -> Data.Ini.keys x ini) iniSections
      textList = foldl (\acc x -> op acc x) [] keysText
      stringList = map unpack textList
  putStr $ intercalate "\n" stringList


op acc x =
  case x of
      Left _ -> acc
      Right y -> acc ++ y

run = do
  input <- getContents
  ini <- parseConfig
  putStr $ ast2String $ Ast.build (ini, sections ini) $ Data.List.lines input


getConfigPath = do
  env <- lookupEnv "SUBS_CONFIG"
  home <- getEnv "HOME"
  case env of
    Just a  -> return a
    Nothing -> return (home ++ "/.subsconfig.ini")

parseConfig = do
  configFile <- getConfigPath
  iniFile <- readIniFile configFile
  case iniFile of
    Left error -> putStr error >> exit
    Right ini  -> return ini
