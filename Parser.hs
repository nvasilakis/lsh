module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim hiding (try)
import qualified Text.ParserCombinators.Parsec.Token as PT
import Types
import Control.Monad

restricted = "!#$%| >()\n\";"

parseComplex :: Parser Complex
parseComplex = try parseNoOp <|>
               try parsePipeSemi <|>
               try parseHigher <|>
               try parseStatement

--
parseNoOp :: Parser Complex
parseNoOp = do
  skipMany space
  eof
  return Noop

parsePipeSemi :: Parser Complex
parsePipeSemi = parserBind base rest
  where base = try parseNoOp <|>
               try parseHigher <|>
               try parseStatement
        rest x = pipe x <|> semi x <|> return x
        pipe x = do
          skipMany space
          char '|'
          y <- base
          skipMany space
          rest $ Pipe x y
        semi x = do
          skipMany space
          char ';'
          skipMany space
          rest $ Semi x

parseHigher :: Parser Complex
parseHigher = do
  skipMany space
  hType <- parseHigherType
  skipMany1 space
  hFst <- parseParens
  skipMany1 space
  hSnd <- parseParens
  skipMany space
  return $ Higher hType hFst hSnd
  
parseParens :: Parser Complex
parseParens = do
  char '('
  inner <- parseComplex
  char ')'
  return $ inner

-- TODO: Make sure you cannot use sth smarter to generate "Higher" (use of Ord?)
parseHigherType :: Parser Higher
parseHigherType = do
  action <- choice $ map (try . string)  ["map", "fold", "filter", "zipWith"]
  case action of
    ("map")     -> return $ Map
    ("fold")    -> return $ Fold
    ("filter")  -> return $ Filter
    ("zipWith") -> return $ ZipWith

parseStatement :: Parser Complex
parseStatement = do
  skipMany space
  stat <- try parseCommand <|> try parseAssign <|> try parseStVal
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
  v <- try parseNumber <|> try parseQuoted <|> try parseString
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
  val <- parseValue
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
