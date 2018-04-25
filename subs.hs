
import Data.Ini
import Data.Text (pack,unpack, splitOn)
import Data.List (unfoldr)
import Text.Printf

main = do

        iniFile <- readIniFile "/home/jean/Dropbox/projects/subs/default.ini"
        input <- getContents
        let
         stdinFirstLine = (head (lines input))
         arguments = (separateBy ' ' stdinFirstLine)
         values = tail arguments
         key = head arguments
        case iniFile of
            Left error ->
                putStr error
            Right ini ->
                putStr (processEntry entry values)
                where
                entry = (getIniEntry ini key)


processEntry base [] = base
processEntry base a =  printf base (head a)


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


