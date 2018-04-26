module Main where

import           Data.Dynamic
import           Data.Ini
import           Data.List         (unfoldr)
import           Data.List
import           Data.String.Utils
import           Data.Text         (pack, splitOn, unpack)
import           Data.Typeable
import           Debug.Trace
import           Text.Printf

main :: IO ()
main = do
  iniFile <- readIniFile "/home/jean/Dropbox/projects/subs/default.ini"
  input <- getContents
  let stdinFirstLine = head (lines input)
      arguments = separateBy ' ' stdinFirstLine
      values = tail arguments
      key = head arguments
  case iniFile of
    Left error -> putStr error
    Right ini -> putStr (processArguments match values)
      where match = (getIniMatch ini key)

processArguments :: String -> [String] -> String
processArguments match arguments =
  case idx of
    Just x -> processArguments (replaced ++ (snd pair)) aTail
      where pair = splitAt' (x + 2) match
            replaced = replace "%s" aHead (fst pair)
    Nothing -> match
  where
    idx = elemIndex '%' match
    aHead = argHead (length arguments) arguments
    aTail = argTail (length arguments) arguments

argTail len arguments =
  if len > 1
    then tail arguments
    else []

argHead :: Int -> [String] -> String
argHead len arguments =
  if len > 0
    then head arguments
    else ""

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
