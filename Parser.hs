module Parser where

import Text.ParserCombinators.Parsec
import Types
import Control.Monad

parseHandler :: Parser Statement
parseHandler = try parseCommand <|> try parseAssign <|> parseStVal

---------- Parse Command
parseCommand :: Parser Statement
parseCommand = do
  (String cmd)  <- parseString
  skipMany1 space
  args <- sepBy parseValue (skipMany1 space)
  return $ Command cmd (args)

---------- Parse Value
parseStVal :: Parser Statement
parseStVal = do
  val <- parseValue
  return $ Val val

parseValue :: Parser Value
parseValue = parseNumber <|> parseQuoted <|> parseString

parseNumber :: Parser Value
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser Value
parseString = do
  str <- many1 (noneOf "!#$%| >")
  return $ String str

parseQuoted :: Parser Value
parseQuoted = do
    char '"'
    x <- many (noneOf "\\\"" <|> parseQuotes) -- any character except \ or "
    char '"'
    return $ Quoted x

parseQuotes :: Parser Char-- parse \\ and \"
parseQuotes = do
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'
      _   -> x

---------- Parse Assign
parseAssign :: Parser Statement
parseAssign = do
  var <- parseAssVar
  char '='
  val <- parseNumber <|> parseQuoted <|> parseString
  return $ Assign var val

parseAssVar :: Parser String
parseAssVar = do
         c <- letter <|> char '_'
         cs <- many (letter <|> digit <|> char '_')
         return (c:cs)
         <?> "Error parsing variable"




---------- Parse Helpers -- used mostly for testing
readHelper :: String -> String -- Read helper for Complex statements
readHelper input = case parse parseHandler "Shell Statement" input of
  Left err -> "No match: " ++ show err
  Right v  -> "Found value" ++ show v

symbol :: Parser Char
symbol = oneOf "!#$%| >"
