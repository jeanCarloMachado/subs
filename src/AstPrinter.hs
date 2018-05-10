module AstPrinter where

import           Data.Char          (isSpace)
import           Data.Dynamic
import           Data.List          (dropWhile, unfoldr)
import           Data.List
import           Data.List.Split    (splitPlaces)
import           Data.Maybe         (catMaybes)
import           Data.String.Utils
import           Data.Text          (Text, pack, splitOn, unpack)
import           Data.Typeable
import           Ast

ast2String :: Ast -> String
ast2String ast = iast2string ast

iast2string :: Ast -> String
iast2string ast = myUnlines $  map node2Str ast

node2Str :: Node -> String
node2Str node =
  case currentMatch of
    Just matchStr -> proc $ matchStr
    Nothing       -> proc $ literal node
  where
    proc =  replaceChildrenPart . processArguments args . processIndentation node
    replaceChildrenPart = replaceChildren $ iast2string $ children node 
    currentMatch = match node
    args = arguments node

-- Indentation related
processIndentation node str =
  (setIndentation node . indentationMark node . dropExistingIndentation . addNewLineAfterChildren ) str

addNewLineAfterChildren :: String -> String
addNewLineAfterChildren =
  replace "%c" "%c\n"

dropExistingIndentation :: String -> String
dropExistingIndentation =
   myUnlines . map (dropWhile isSpace) . lines


indentationMark :: Node -> String -> String
indentationMark node =
  replace "%i" spaces
  where
   spaces =  (indentation node) `replicate` ' '

setIndentation :: Node -> String -> String
setIndentation node =
  myUnlines . map (spaces ++) . lines
  where
   spaces =  indentation node `replicate` ' '

-- Arguments related
processArguments ::  Arguments -> MatchSnippet -> String
processArguments arguments matchSnippet =
  (positionalPartial . numericPartial) matchSnippet
  where
  numericPartial = (numericArgs arguments)
  positionalPartial = (positionalArgs arguments)


numericArgs ::  Arguments -> MatchSnippet -> String
numericArgs arguments matchSnippet = replaceNumeric arguments matchSnippet 1

replaceNumeric :: Arguments -> MatchSnippet -> Int -> String
replaceNumeric arguments matchSnippet argumentIndice =
  if argumentIndice < argsLen
  then replaceNumeric arguments result $ argumentIndice + 1
  else result
  where
    argsLen = length (arguments)
    currentArgument = arguments !! (argumentIndice -1 )
    result = replace ("%" ++ (show argumentIndice)) currentArgument matchSnippet

positionalArgs :: Arguments -> MatchSnippet -> String
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



argTail arguments =
  conditionalVal ((length arguments) > 1) (tail arguments) []

argHead arguments =
  conditionalVal ((length arguments) > 0) (head arguments) ""


replaceChildren :: String -> String -> String
replaceChildren "" = replace "%c\n" ""
replaceChildren children = replace "%c" (children)

--util

conditionalVal predicate trueVal falseVal =
  if predicate
  then trueVal
  else falseVal


substringP :: String -> String -> Maybe Int
substringP _ [] = Nothing
substringP sub str =
  case isPrefixOf sub str of
    False -> fmap (+ 1) $ substringP sub (tail str)
    True  -> Just 0


