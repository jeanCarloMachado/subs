module Main where

import           Data.Dynamic
import           Data.Ini
import           Data.List         (unfoldr, dropWhile)
import           Data.List
import           Data.String.Utils
import           Data.Text         (pack, splitOn, unpack)
import           Data.Typeable
import           Debug.Trace
import           Text.Printf
import           Data.Char  (isSpace)

main :: IO ()
main = do
  iniFile <- readIniFile "/home/jean/Dropbox/projects/subs/default.ini"
  input <- getContents
  case iniFile of
    Left error -> putStr error
    Right ini -> putStr (processAll ini input)

processAll ini input =
  if linesCount > 1
 then
     rootIdentationLevel (addChildren rootProcess (intercalate "\n    " (map (parseLine ini) (tail (lines input))))) input
  else
    rootIdentationLevel rootProcess input
    
  where
      linesCount = length (lines input)
      rootProcess = parseLine ini (lines input !! 0)


parseLine ini line = 
    processUnit (getIniMatch ini (head (getArguments (line)))) (tail (getArguments (line)))

rootIdentationLevel result input  =
  replace "\\n" ("\\n" ++ takeWhile isSpace input) ( (takeWhile isSpace input) ++ result)


addChildren :: String -> String -> String
addChildren parent children =
  replace "%c" (replace "\\n" "\\n    " ("    " ++ children)) parent


getArguments input =
       separateBy ' ' (dropWhile isSpace input)

processUnit :: String -> [String] -> String
processUnit match arguments =
  case idx of
    Just x -> processUnit (replaced ++ (snd pair)) aTail
      where pair = splitAt' (x + 2) match
            replaced = replace "%s" aHead (fst pair)
    Nothing -> match
  where
    idx = substringP "%s" match
    aHead = argHead arguments
    aTail = argTail arguments

argTail arguments =
  if len > 1
    then tail arguments
    else []
  where
    len = (length arguments)

argHead arguments =
  if len > 0
    then head arguments
    else ""
  where
    len = (length arguments)

getIniMatch ini value =
  case either of
    Left msg   -> error msg
    Right text -> unpack text
  where
    either = lookupValue (pack "global") (pack value) ini

separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep
  where
    sep [] = Nothing
    sep l  = Just . fmap (drop 1) . break (== chr) $ l

splitAt' = \n -> \xs -> (take n xs, drop n xs)

substringP :: String -> String -> Maybe Int
substringP _ []  = Nothing
substringP sub str = case isPrefixOf sub str of
  False -> fmap (+1) $ substringP sub (tail str)
  True  -> Just 0
