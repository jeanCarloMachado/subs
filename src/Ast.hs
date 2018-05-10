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
  , identation :: Int
  } deriving (Show)

type MatchSnippet = String
type Ast = [Node]
type IniCfg = (Ini, [Text])

-- BUILD AST

build :: IniCfg -> [String] -> Ast
build inicfg input = map (string2Node inicfg) sameLevel
  where
    sameLevel = getSiblingsWithNestedChildren (head input) input

string2Node :: IniCfg -> [String] -> Node
string2Node inicfg literal =
  Node
  { match = getIniMatch (fst inicfg) (snd inicfg) function
  , function = function
  , arguments = tail values
  , literal = literalLine
  , children = build inicfg $ tail literal
  , identation = length $ takeWhile isSpace literalLine
  }
  where
    function = head values
    values = separateBy ' '  $ dropWhile isSpace literalLine
    literalLine = head literal

getSiblingsWithNestedChildren :: String -> [String] -> [[String]]
getSiblingsWithNestedChildren entry entries =
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

-- PRINT AST

ast2String :: Ast -> String
ast2String ast = unlines $ splitStr "\\n" $ iast2string ast

iast2string ast = intercalate "\\n" $ map node2Str ast

node2Str :: Node -> String
node2Str node =
  case currentMatch of
    Nothing       -> proc $ literal node
    Just matchStr -> proc $ processArguments matchStr $ arguments node
  where
    proc = (\x -> replaceChildren (addIdentation node x) $ iast2string $ children node )
    currentMatch = match node

processArguments :: MatchSnippet -> [String] -> String
processArguments matchSnippet arguments =
  processPositionalArguments numericArgumentsProcessed arguments
  where
  numericArgumentsProcessed = processNumericArguments matchSnippet arguments 



processNumericArguments :: MatchSnippet -> [String] -> String
processNumericArguments matchSnippet arguments = recursiveRelaceNumeric arguments matchSnippet 1

recursiveRelaceNumeric arguments matchSnippet argumentIndice =
  if argumentIndice < argsLen
  then recursiveRelaceNumeric arguments result $ argumentIndice + 1
  else result
  where
    argsLen = length (arguments)
    currentArgument = arguments !! (argumentIndice -1 )
    result = replace ("%" ++ (show argumentIndice)) currentArgument matchSnippet


processPositionalArguments :: MatchSnippet -> [String] -> String
processPositionalArguments matchSnippet arguments =
  case idx of
    Just x -> recurse $ replace "%s" aHead (fst pair)
      where pair = splitAt' (x + 2) matchSnippet
            recurse = \x -> processPositionalArguments (x  ++ (snd pair)) aTail
    Nothing -> matchSnippet
  where
    idx = substringP "%s" matchSnippet --first occurence of %s
    aHead = argHead arguments
    aTail = argTail arguments


replaceChildren :: String -> String -> String
replaceChildren parent ""       = replace "%c" "" parent
replaceChildren parent children = replace "%c" children parent

addIdentation :: Node -> String -> String
addIdentation node str =
  replace "\\n" ("\\n" ++ spaces) (spaces ++ str)
  where
   spaces =  identation node `replicate` ' '


argTail arguments =
  conditionalVal ((length arguments) > 1) (tail arguments) []

argHead arguments =
  conditionalVal ((length arguments) > 0) (head arguments) ""

conditionalVal predicate trueVal falseVal =
  if predicate
  then trueVal
  else falseVal

getChildren :: String -> [String] -> [String]
getChildren ref entries =
  [head entries] ++
  takeWhile (\x -> not (hasSameIdentationLevel ref x)) (tail entries)

hasSameIdentationLevel :: String -> String -> Bool
hasSameIdentationLevel current next =
  if identationInNext == identationInCurrent
    then True
    else False
  where
    identationInCurrent = length $ takeWhile isSpace current
    identationInNext = length $ takeWhile isSpace next


separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep
  where
    sep [] = Nothing
    sep l  = Just . fmap (drop 1) . break (== chr) $ l


splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _   []  subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise            = split' sub (tail str) (head str:subacc) acc


splitAt' = \n -> \xs -> (take n xs, drop n xs)

substringP :: String -> String -> Maybe Int
substringP _ [] = Nothing
substringP sub str =
  case isPrefixOf sub str of
    False -> fmap (+ 1) $ substringP sub (tail str)
    True  -> Just 0
