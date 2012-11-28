{- credit for code to http://www.serpentine.com/blog/2007/01/31/parsing-a-simple-config-file-in-haskell/ -}
module ConfigFile (
                     Config,
                     readConfig
                   )where
import Data.Char
import Control.Monad
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Data.Either
import Data.Maybe

type Config = Map.Map String String

tokenparser :: Parser String
tokenparser = do
         c <- letter <|> char '_'
         cs <- many (letter <|> digit <|> char '_')
         return (c:cs)
         <?> "identifier"

commentparser :: Parser ()
commentparser = do char '#'
                   skipMany (noneOf "\r\n")
                <?> "comment"

eolparser :: Parser ()
eolparser = do oneOf "\n\r"
               return ()
             <?> "end of line"

item :: Parser (String, String)
item = do 
           key <- tokenparser
           skipMany space
           char '='
           skipMany space
           value <- manyTill anyChar (try eolparser <|> try commentparser <|> eof)
           return (key, rstrip value)
     where rstrip = reverse . dropWhile isSpace . reverse

line :: Parser (Maybe (String,String))
line = do skipMany space
          try (commentparser >> return Nothing) <|> (item >>= return . Just)

file :: Parser [(String, String)]

file = do lines <- many line 
          return (catMaybes lines)

readConfig:: SourceName -> IO (Either ParseError Config)

readConfig name = parseFromFile file name >>= 
                  return .fmap (foldr(uncurry Map.insert) Map.empty .reverse)
           


