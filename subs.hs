
import Data.Ini
import Data.HashMap.Strict
import Data.Text

main = do

        iniFile <- readIniFile "/home/jean/Dropbox/projects/subs/default.ini"
        case iniFile of
            Left error ->
                putStrLn error
            Right val ->
                putStrLn ( getValue val  )


getValue ini =
    case either of
        Left error ->
            error
        Right text ->
            ( unpack text )

    where
        either = lookupValue (pack "global") (pack "fn") ini
