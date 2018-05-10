module Ast where

import           Data.Char          (isSpace)
import           Data.Dynamic
import           Data.Ini
import           Data.List          (dropWhile, unfoldr)
import           Data.List
import           Data.List.Split    (splitPlaces)
import           Data.Maybe         (catMaybes)
import           Data.String.Utils
import           Data.Text          (Text, pack, splitOn, unpack)
import           Data.Typeable

data Node = Node
  { literal    :: String
  , function   :: String
  , arguments  :: [String]
  , match      :: Maybe String
  , children   :: [Node]
  , indentation :: Int
  } deriving (Show)

type MatchSnippet = String
type Ast = [Node]
type Config = (Ini, [Text])

build :: Config -> [String] -> Ast
build inicfg input = map (string2Node inicfg) sameLevel
  where
    sameLevel = siblingsWithChildren (head input) input

string2Node :: Config -> [String] -> Node
string2Node inicfg literal =
  Node
  { match = getIniMatch (fst inicfg) (snd inicfg) function
  , function = function
  , arguments = tail values
  , literal = literalLine
  , children = build inicfg $ tail literal
  , indentation = length $ takeWhile isSpace literalLine
  }
  where
    function = head values
    values = separateBy ' '  $ dropWhile isSpace literalLine
    literalLine = head literal

siblingsWithChildren :: String -> [String] -> [[String]]
siblingsWithChildren entry entries =
  [] ++
  (map
     (\indice -> getChildren entry $ snd $ splitAt' indice entries)
     parentsIndices)
  where
    parents = filter (hasSameIdentationLevel entry) entries
    parentsIndicesMaybe = map (\x -> elemIndex x entries) parents
    parentsIndices = catMaybes parentsIndicesMaybe


getIniMatch ini [] value =  Nothing
getIniMatch ini sections value =
  case either of
    Left msg   -> getIniMatch ini (tail sections) value
    Right text -> Just (unpack text)
  where
    either = lookupValue (head sections) (pack value) ini

getChildren :: String -> [String] -> [String]
getChildren ref entries =
  [head entries] ++
  takeWhile (\x -> not (hasSameIdentationLevel ref x)) (tail entries)


separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep
  where
    sep [] = Nothing
    sep l  = Just . fmap (drop 1) . break (== chr) $ l

hasSameIdentationLevel :: String -> String -> Bool
hasSameIdentationLevel current next =
  if identationInNext == identationInCurrent
    then True
    else False
  where
    identationInCurrent = length $ takeWhile isSpace current
    identationInNext = length $ takeWhile isSpace next

splitAt' = \n -> \xs -> (take n xs, drop n xs)
