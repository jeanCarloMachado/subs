module Main where

import           Data.Dynamic
import           Data.Ini
import           Data.List         (unfoldr, dropWhile )
import           Data.List
import           Data.List.Split (splitPlaces)
import           Data.String.Utils
import           Data.Text         (pack, splitOn, unpack)
import           Data.Typeable
import           Debug.Trace
import           Text.Printf
import           Data.Char  (isSpace)
import           Data.Maybe (catMaybes)

type Instruction = String
type Arguments = [String]
data Statement = Instruction Arguments

main :: IO ()
main = do
  iniFile <- readIniFile "/home/jean/Dropbox/projects/subs/default.ini"
  input <- getContents
  case iniFile of
    Left error -> putStr error
    Right ini -> putStr (processAll ini (lines input))

processAll :: Ini -> [String] -> String
processAll ini input =
      intercalate "\n" (agregateNodes ini input)


agregateNodes :: Ini -> [String] -> [String]
agregateNodes ini lines =
  map (\x -> copyIdentationLevel firstLine (processNode ini x)) sameLevel
  where
      firstLine = (head lines)
      sameLevel = getSameLevelWithChildren firstLine lines

processNode ini lines =
  addChild (getNodeValue ini line) (intercalate "\n" (agregateNodes ini (tail lines)))
 where
  line = head lines

getNodeValue :: Ini -> String -> String
getNodeValue ini line =
    if match == ""
    then line
    else  processArguments match arguments
    where
      match = getIniMatch ini (head values)
      values = getLineValues line
      arguments = tail values

getSameLevelWithChildren :: String -> [String] ->[[String]]
getSameLevelWithChildren entry entries =
 [] ++  (map (\indice -> getChildren entry (snd (splitAt' indice entries))) parentsIndices)
  where
  parents = filter (hasSameIdentationLevel entry) entries
  parentsIndicesMaybe = map (\x -> elemIndex x entries) parents
  parentsIndices = catMaybes parentsIndicesMaybe

getChildren :: String -> [String] -> [String]
getChildren ref entries =
    [head entries] ++ takeWhile (\x -> not (hasSameIdentationLevel ref x)) (tail entries)



hasSameIdentationLevel :: String -> String -> Bool
hasSameIdentationLevel current next =
    if identationInNext == identationInCurrent
    then True
    else False
  where
    identationInCurrent = length (takeWhile isSpace current)
    identationInNext = length (takeWhile isSpace next)

nextIsChild current next =
    if identationInNext > identationInCurrent
    then True
    else False
  where
    identationInCurrent = length (takeWhile isSpace current)
    identationInNext = length (takeWhile isSpace next)

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


copyIdentationLevel :: String -> String -> String
copyIdentationLevel compare result =
  replace "\\n" ("\\n" ++ compareSpaces) (compareSpaces ++ result)
  where
  compareSpaces = takeWhile isSpace compare


addChild :: String -> String -> String
addChild parent   ""  = replace "%c" "" parent
addChild parent children =
  replace "%c"  children parent


getLineValues input =
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
