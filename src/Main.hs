module Main where

import           Data.Char          (isSpace)
import           Data.Dynamic
import           Data.Ini
import           Data.List          (dropWhile, unfoldr)
import           Data.List
import           Data.List.Split    (splitPlaces)
import           Data.Maybe         (catMaybes)
import           Data.String.Utils
import           Data.Text          (pack, splitOn, unpack)
import           Data.Typeable
import           Debug.Trace
import           System.Environment
import           Text.Printf

data Node = Node
  { literal    :: String
  , function   :: String
  , arguments  :: [String]
  , match      :: Maybe String
  , children   :: [Node]
  , identation :: Int
  } deriving (Show)

type Ast = [Node]

main :: IO ()
main = do
  input <- getContents
  configFile <- getConfigPath
  iniFile <- readIniFile configFile
  case iniFile of
    Left error -> putStr error
    Right ini  -> putStr (processAll ini (lines input))

getConfigPath = do
  env <- lookupEnv "SUBS_CONFIG"
  home <- getEnv "HOME"
  case env of
    Just a  -> return a
    Nothing -> return (home ++ "/.subsconfig.ini")

processAll :: Ini -> [String] -> String
processAll ini input = ast2String (buildAst ini input)


buildAst :: Ini -> [String] -> Ast
buildAst ini input = map (string2Node ini) sameLevel
  where
    sameLevel = getSiblingsWithNestedChildren (head input) input

string2Node :: Ini -> [String] -> Node
string2Node ini literal =
  Node
  { match = getIniMatch ini function
  , function = function
  , arguments = (tail values)
  , literal = literalLine
  , children = buildAst ini (tail literal)
  , identation = length (takeWhile isSpace literalLine)
  }
  where
    function = head (values)
    values = separateBy ' ' (dropWhile isSpace literalLine)
    literalLine = (head literal)

ast2String :: Ast -> String
ast2String ast = intercalate "\n" (map node2Str ast)

node2Str :: Node -> String
node2Str node =
  case currentMatch of
    Nothing       -> addChild (addIdentation node (literal node)) (ast2String (children node))
    Just matchStr -> addChild (addIdentation node (processArguments matchStr (arguments node)) )  (ast2String (children node))
  where
    currentMatch = match node

addChild :: String -> String -> String
addChild parent ""       = replace "%c" "" parent
addChild parent children = replace "%c" children parent


addIdentation :: Node -> String -> String
addIdentation node str =
  replace "\\n" ("\\n" ++ spaces) (spaces ++ str)
  where
   spaces = replicate (identation node) ' '


processArguments :: String -> [String] -> String
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

getSiblingsWithNestedChildren :: String -> [String] -> [[String]]
getSiblingsWithNestedChildren entry entries =
  [] ++
  (map
     (\indice -> getChildren entry (snd (splitAt' indice entries)))
     parentsIndices)
  where
    parents = filter (hasSameIdentationLevel entry) entries
    parentsIndicesMaybe = map (\x -> elemIndex x entries) parents
    parentsIndices = catMaybes parentsIndicesMaybe

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
    identationInCurrent = length (takeWhile isSpace current)
    identationInNext = length (takeWhile isSpace next)



getIniMatch ini value =
  case either of
    Left msg   -> Nothing
    Right text -> Just (unpack text)
  where
    either = lookupValue (pack "global") (pack value) ini

separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep
  where
    sep [] = Nothing
    sep l  = Just . fmap (drop 1) . break (== chr) $ l

splitAt' = \n -> \xs -> (take n xs, drop n xs)

substringP :: String -> String -> Maybe Int
substringP _ [] = Nothing
substringP sub str =
  case isPrefixOf sub str of
    False -> fmap (+ 1) $ substringP sub (tail str)
    True  -> Just 0
