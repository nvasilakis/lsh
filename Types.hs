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

newtype Variable = V String
newtype Args = A [Value]

data Statement =
    Command String Args       -- echo "a b"
  | Val Value
  | Assign Variable Value     -- x = e

{-
data Expression =
    Var Variable                    -- x
  | Val Value                       -- v
-}

data Value =
    Number Integer -- 3
  | String String  -- abcd
  | Quoted String  -- "ab cde"

main :: IO ()
main = do args <- getArgs
          putStrLn (readStat (args !! 0))

readStat :: String -> String
readStat input = case parse parseStat "Shell Statement" input of
  Left err -> "No match: " ++ show err
  Right _  -> "Found value"

parseStat :: Parser Statement
parseStat = parseStVal <|> parseCommand
            -- <|> parseAssign

parseStVal :: Parser Statement
parseStVal = do
  val <- parseNumber <|> parseString <|> parseQuoted
  return $ Val val

symbol :: Parser Char
symbol = oneOf "!#$%| >"

parseCommand :: Parser Statement
parseCommand = do
  cmd  <- many (noneOf "!#$%| >")
  args <- sepBy parseValue spaces
  return $ Command cmd (A args)

parseAssign :: Parser Statement
parseAssign = undefined

parseValue :: Parser Value
parseValue = parseNumber <|> parseString <|> parseQuoted

parseNumber :: Parser Value
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser Value
parseString = do
  str <- many (noneOf "!#$%| >")
  return $ String str

parseQuoted :: Parser Value
parseQuoted = do
    char '"'
    x <- many (noneOf "\\\"" <|> parseQuotes)
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