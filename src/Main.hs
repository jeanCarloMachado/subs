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

processAll :: Ini -> String -> String
processAll ini input =
     copyIdentationLevel rootProcess input
  where
      rootProcess = processNode ini (lines input)

processNode _  [] =  ""
processNode ini linesToProcess =
    if match == ""
    then addSibling literalLine nextNodes
    else add linesToProcess nodeStr nextNodes
    where
      currentLine  = linesToProcess !! 0
      literalLine = currentLine
      match = getIniMatch ini (head (getArguments (currentLine)))
      arguments = tail (getArguments (currentLine))
      nodeStr = processArguments  match arguments
      nextNodes = processNode ini (tail linesToProcess)

add :: [String] -> String -> String -> String
add lines current next =
   if nextIsChild lines
   then addChild current next
   else addSibling current next


nextIsChild [x] = True
nextIsChild lines =
    if identationInNext > identationInCurrent
    then True
    else False
  where
    currentLine  = lines !! 0
    identationInCurrent = length (takeWhile isSpace currentLine)
    nextLine  = lines !! 1
    identationInNext = length (takeWhile isSpace nextLine)


processArguments match arguments =
  case idx of
    Just x -> processArguments (replaced ++ (snd pair)) aTail
      where pair = splitAt' (x + 2) match
            replaced = replace "%s" aHead (fst pair)
    Nothing -> match
  where
    idx = substringP "%s" match
    aHead = argHead arguments
    aTail = argTail arguments


copyIdentationLevel result compare  =
  replace "\\n" ("\\n" ++ compareSpaces) (compareSpaces ++ result)
  where
  compareSpaces = takeWhile isSpace compare

addSibling a b =
  newA ++ copyIdentationLevel ("\\n" ++ b) a
  where
    newA = replace "%c" "" a


addChild :: String -> String -> String
addChild parent   ""  = replace "%c" "" parent
addChild parent children =
  replace "%c" (replace "\\n" "\\n    " ("    " ++ children)) parent


getArguments input =
       separateBy ' ' (dropWhile isSpace input)


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
    Left msg   -> "" 
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
