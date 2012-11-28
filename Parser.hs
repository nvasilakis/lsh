module Parser where

import Text.ParserCombinators.Parsec
import Types
import Control.Monad

readStat :: String -> String
readStat input = case parse parseStat "Shell Statement" input of
  Left err -> "No match: " ++ show err
  Right v  -> "Found value" ++ show v

parseStat :: Parser Statement
parseStat = try parseCommand <|> parseStVal
            -- <|> parseAssign

parseStVal :: Parser Statement
parseStVal = do
  val <- parseNumber <|> parseQuoted <|> parseString
  return $ Val val

symbol :: Parser Char
symbol = oneOf "!#$%| >"

parseCommand :: Parser Statement
parseCommand = do
  (String cmd)  <- parseString
  skipMany1 space
  args <- sepBy parseValue (skipMany1 space)
--  return $ Command cmd ([ String "one"])
  return $ Command cmd (args)

parseAssign :: Parser Statement
parseAssign = undefined

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

-- parse \\ and \"
parseQuotes :: Parser Char
parseQuotes = do
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'
      _   -> x
