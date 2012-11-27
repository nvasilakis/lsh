module Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
import qualified Text.PrettyPrint.HughesPJ as PP

import Text.ParserCombinators.Parsec
import System.Environment

import Control.Monad.Error
import Control.Monad.State

-- Syntax of Shell language

type Variable = String
type Args = [Value]

data Statement =
    Command String Args       -- echo "a b"
  | Val Value
  | Assign Variable Value     -- x = e
    deriving Show

{-
data Expression =
    Var Variable                    -- x
  | Val Value                       -- v
-}

data Value =
    Number Integer -- 3
  | String String  -- abcd
  | Quoted String  -- "ab cde"
    deriving Show

main :: IO ()
main = do args <- getArgs
          putStrLn (readStat (args !! 0))

readStat :: String -> String
readStat input = case parse parseStat "Shell Statement" input of
  Left err -> "No match: " ++ show err
  Right v  -> "Found value" ++ show v

parseStat :: Parser Statement
parseStat = --parseStVal
            -- <|>
             parseCommand
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
  args <- sepBy parseValue (skipMany1 space)
--  return $ Command cmd ([ String "one"])
  return $ Command cmd (tail args)

parseAssign :: Parser Statement
parseAssign = undefined

parseValue :: Parser Value
parseValue = parseNumber <|> parseQuoted <|> parseString

parseNumber :: Parser Value
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser Value
parseString = do
  str <- many (noneOf "!#$%| >")
  return $ String str

parseQuoted :: Parser Value
parseQuoted = do
    char '"'
    x <- many (noneOf "\\\"" <|> parseQuotes) -- any character except \ or ""
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

-- Pretty printing for the Shell language