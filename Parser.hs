module Parser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as PT
import Types
import Control.Monad

restricted = "!#$%| >()\n"

parseComplex :: Parser Complex
parseComplex = try parseNoOp
              <|> try parseHigher
              <|> try parsePipe
              <|> parseStatement
--            <|> parseSemi
--
parseNoOp :: Parser Complex
parseNoOp = do
  skipMany space
  eof
  return Noop

parsePipe :: Parser Complex
parsePipe = do
  skipMany space
  s1 <- parseStatement
  skipMany space
  char '|'
  skipMany space
  s2 <- parseStatement
  skipMany space
  return $ Pipe s1 s2

parseHigher :: Parser Complex
parseHigher = do
  skipMany space
  hType <- parseHigherType
  skipMany1 space
  hFst <- parseParens
  skipMany1 space
  hSnd <- parseParens
  return $ Higher hType hFst hSnd

parseParens :: Parser Complex
parseParens = do
  char '('
  inner <- parseStatement
  char ')'
  return $ inner

-- TODO: Make sure you cannot use sth smarter to generate "Higher" (use of Ord?)
parseHigherType :: Parser Higher
parseHigherType = do
  action <- choice $ map string ["map", "fold", "filter", "zipWith"]
  case action of
    ("map")     -> return $ Map
    ("fold")    -> return $ Fold
    ("filter")  -> return $ Filter
    ("zipWith") -> return $ ZipWith

parseStatement :: Parser Complex
parseStatement = do
  skipMany space
  stat <- try parseCommand <|> try parseAssign <|> parseStVal
  return $ Statement stat

---------- Parse Command
parseCommand :: Parser Statement
parseCommand = do
  (String cmd)  <- parseString
  skipMany1 space
  args <- (many1 parseValue)
  return $ Command cmd (args)

---------- Parse Value
parseStVal :: Parser Statement
parseStVal = do
  val <- parseValue
  return $ Val val

parseValue :: Parser Value
parseValue = do
  skipMany space
  v <-  parseNumber <|> parseQuoted <|> parseString
  skipMany space
  return v

parseNumber :: Parser Value
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser Value
parseString = do
  skipMany space
  str <- many1 (noneOf restricted)
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
r :: String -> String -- Read helper for Complex statements
r input = case parse parseComplex "Shell Statement" input of
  Left err -> "No match: " ++ show err
  Right v  -> "Found value" ++ show v

symbol :: Parser Char
symbol = oneOf restricted
