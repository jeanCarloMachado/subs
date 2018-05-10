module AstPrinter where

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
import           Ast

ast2String :: Ast -> String
ast2String ast = unlines $ splitStr "\\n" $ iast2string ast

iast2string :: Ast -> String
iast2string ast = intercalate "\\n" $  map node2Str ast

node2Str :: Node -> String
node2Str node =
  case currentMatch of
    Just matchStr -> proc $ processArguments matchStr $ arguments node
    Nothing       -> literal node
  where
    proc =  replaceChildrenPart . indentationMarkPart . indentedNode
    replaceChildrenPart = replaceChildren processedChildren
    processedChildren =  iast2string $ children node 
    indentationMarkPart = indentationMark node
    indentedNode = addIdentation node
    currentMatch = match node

indentationMark node str =
  replace "%i" spaces str
  where
   spaces =  indentation node `replicate` ' '

processArguments :: MatchSnippet -> [String] -> String
processArguments matchSnippet arguments =
  (positionalPartial . numericPartial) matchSnippet
  where
  numericPartial = (numericArgs arguments)
  positionalPartial = (positionalArgs arguments)


numericArgs ::  [String] -> MatchSnippet -> String
numericArgs arguments matchSnippet = replaceNumeric arguments matchSnippet 1

replaceNumeric :: [String] -> MatchSnippet -> Int -> String
replaceNumeric arguments matchSnippet argumentIndice =
  if argumentIndice < argsLen
  then replaceNumeric arguments result $ argumentIndice + 1
  else result
  where
    argsLen = length (arguments)
    currentArgument = arguments !! (argumentIndice -1 )
    result = replace ("%" ++ (show argumentIndice)) currentArgument matchSnippet


positionalArgs :: [String] -> MatchSnippet -> String
positionalArgs arguments matchSnippet =
  case idx of
    Just x -> recurse $ replace "%s" aHead (fst pair)
      where pair = splitAt' (x + 2) matchSnippet
            recurse = \x -> positionalArgs aTail (x  ++ (snd pair)) 
    Nothing -> matchSnippet
  where
    idx = substringP "%s" matchSnippet --first occurence of %s
    aHead = argHead arguments
    aTail = argTail arguments


replaceChildren :: String -> String -> String
replaceChildren "" = replace "%c" ""
replaceChildren children = replace "%c" children

addIdentation :: Node -> String -> String
addIdentation node nodeResult =
  replace "\\n" ("\\n" ++ spaces) (spaces ++ nodeResult)
  where
   spaces =  indentation node `replicate` ' '


argTail arguments =
  conditionalVal ((length arguments) > 1) (tail arguments) []

argHead arguments =
  conditionalVal ((length arguments) > 0) (head arguments) ""

conditionalVal predicate trueVal falseVal =
  if predicate
  then trueVal
  else falseVal

splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _   []  subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise            = split' sub (tail str) (head str:subacc) acc



substringP :: String -> String -> Maybe Int
substringP _ [] = Nothing
substringP sub str =
  case isPrefixOf sub str of
    False -> fmap (+ 1) $ substringP sub (tail str)
    True  -> Just 0
