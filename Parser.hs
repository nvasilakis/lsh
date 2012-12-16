module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim hiding (try)
import qualified Text.ParserCombinators.Parsec.Token as PT
import Types
import Control.Monad
import qualified Data.Map as Map

restricted = "!#$%| >()\n\";"

parseComplete :: Parser Complete
parseComplete = parserBind base rest 
  where base = try parseCcomplex
        rest x = ssep x <|> return x
        ssep x = do
          skipMany space
          char ';'
          c <- try (parseSepSemi x) <|> 
               try (parseSep x)
          rest c

parseCcomplex :: Parser Complete
parseCcomplex = do
  skipMany space
  c <- parseComplex
  skipMany space
  return $ Complex c
  
parseSepSemi :: Complete -> Parser Complete
parseSepSemi c1 = do
  skipMany space
  string ""
  skipMany space
  eof
  return $ Semi c1
  
parseSep :: Complete -> Parser Complete
parseSep c1 = do
  c2 <- parseCcomplex
  return $ Ssep c1 c2

parseComplex :: Parser Complex
parseComplex = try parseNoOp <|>
               try parsePipe <|>
               try parseHigher <|>
               try parseStatement

--
parseNoOp :: Parser Complex
parseNoOp = do
  skipMany space
  eof
  return Noop

parsePipe :: Parser Complex
parsePipe = parserBind base rest
  where base = try parseNoOp <|>
               try parseHigher <|>
               try parseStatement
        rest x = pipe x <|> return x
        pipe x = do
          skipMany space
          char '|'
          y <- base
          skipMany space
          rest $ Pipe x y

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
    
parseSquoted :: Parser Value
parseSquoted = do
    char '\''
    x <- many (noneOf "\\\'" <|> parseQuotes) -- any character except \ or '
    char '\''
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

---------- Parse Dollar Sign
parseDollar :: Uni -> Parser String
parseDollar uni = do
  start <- many(noneOf "$")
  char '$'
  var <- many(noneOf "$")
  case (Map.lookup var $ variables uni) of
    (Just val) -> return $ start ++ show val
    (Nothing) -> return start

replaceDollar :: Uni -> String -> String
replaceDollar uni input = 
  case (parse (many $ parseDollar uni) "" input) of
       Left err -> input
       Right s  -> concat s

---------- Parse Alias Arguments
parseAliasArgs :: Parser (Variable,Value)
parseAliasArgs = do
  skipMany space
  var <- parseAssVar
  char '='
  val <- parseSquoted
  skipMany space
  return (var,val)
  
--parseAliasQuotes
  
---------- Parse Helpers -- used mostly for testing
r :: String -> String -- Read helper for Complex statements
r input = case parse parseComplex "Shell Statement" input of
  Left err -> "No match: " ++ show err
  Right v  -> "Found value" ++ show v

symbol :: Parser Char
symbol = oneOf restricted