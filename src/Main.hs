module Main where

import           Data.Ini
import           System.Environment
import           Ast



main :: IO ()
main = do
  input <- getContents
  configFile <- getConfigPath
  iniFile <- readIniFile configFile
  case iniFile of
    Left error -> putStr error
    Right ini  -> putStr $ processAll ini $ lines input

getConfigPath = do
  env <- lookupEnv "SUBS_CONFIG"
  home <- getEnv "HOME"
  case env of
    Just a  -> return a
    Nothing -> return (home ++ "/.subsconfig.ini")

processAll :: Ini -> [String] -> String
processAll ini input = ast2String (Ast.build ini input)





