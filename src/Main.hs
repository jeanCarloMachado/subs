module Main where

import Data.Ini
import Data.Text (pack,unpack, splitOn)
import Data.List (unfoldr)
import Text.Printf
import Data.Dynamic
import Data.Typeable
import Data.List
import Debug.Trace

main :: IO ()
main = do
        iniFile <- readIniFile "/home/jean/Dropbox/projects/subs/default.ini"
        input <- getContents
        let
         stdinFirstLine = head (lines input)
         arguments = separateBy ' ' stdinFirstLine
         values = tail arguments
         key = head arguments
        case iniFile of
            Left error ->
                putStr error
            Right ini ->
                putStr (processEntry match values)
                where
                match = (getIniEntry ini key)


processEntry :: String -> [String] -> String
processEntry match [] = match
processEntry match (x:xs) =
        processEntry (getPart match x) xs

getPart match arg =
    case idx of
        Just x ->
                replaced  ++ (snd pair)
            where
                pair = splitAt' (x+2) match
                replaced = printf (fst pair) arg
        Nothing ->
            match
    where
        idx = elemIndex '%' match


getIniEntry ini value =
    case either of
        Left msg ->
            error msg
        Right text ->
             unpack text

    where
        either = lookupValue (pack "global") (pack value) ini


separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l


splitAt' = \n -> \xs -> (take n xs, drop n xs)

