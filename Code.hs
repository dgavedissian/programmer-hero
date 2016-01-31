module Code where
import ParserC

data Code = Token String | Indent String

loadTokens :: FilePath -> IO [Code]
loadTokens source = do
    f <- readFile source
    case runParser tokenise f of
      Right r -> return r
      Left err -> print err >> return []

runParser :: ParserC () a -> String -> Either ParserError a
runParser p s = execP p st
    where st = ParserState {
                    remaining  = s,
                    location   = (1, 0),
                    indent     = 0,
                    reserved   = [],
                    extension  = ()
                }

-- Parsing stuff
token :: ParserC () Code
token = Token <$> some (noneOf " \t\n")

indentation :: ParserC () Code
indentation = Indent <$> some whitespace

tokenise :: ParserC () [Code]
tokenise = many $ token +++ indentation
