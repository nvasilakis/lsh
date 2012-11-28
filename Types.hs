module Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import System.IO

--import Help
import qualified Help as H

import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
import qualified Text.PrettyPrint.HughesPJ as PP

import Text.ParserCombinators.Parsec
import System.Environment

import System.Cmd
import System.Process
import Control.Monad.Error
import Control.Monad.State

import GHC.IO.Exception

import System.Exit
import System.Directory
-- Syntax of Shell language

type Variable = String
type Args = [Value]

data Statement =
    Command String Args       -- echo "a b"
  | Val Value                 -- 3 or string or "quoted String"
  | Assign Variable Value     -- Assign x 3, Assign, x "quoted String"
--  deriving Show
{-
data Expression =
    Var Variable                    -- x
  | Val Value                       -- v
-}

data Value =
    Number Integer -- 3
  | String String  -- abcd
  | Quoted String  -- "ab cde"
    deriving (Eq)

showVal :: Statement -> String
showVal (Command cmd args) =  "Running " ++ cmd ++ " | " ++ show args
-- showVal (Command cmd args) =  testSystem cmd "test"

showVal (Val val) = show val
{-}
showVal (Val (String x)) = show x
showVal (Val (Number x)) = show x
showVal (Val (Quoted x)) = show x
-}
showVal (Assign var val) = (show var) ++ " = " ++ (show val)

showV (String x) =  x
showV (Number x) = show x
showV (Quoted x) = show x


instance Show Statement where show = showVal

instance Show Value where show = showV

-- testSystem :: String -> [String] -> String

-- String -> [String] -> Either String
testSystem cmd args = do
  x <- (rawSystem cmd args)
  case x of
    GHC.IO.Exception.ExitSuccess     -> return $ "success"
    GHC.IO.Exception.ExitFailure err -> return $ "error" ++ (show err)


inExecTable :: [(String, [Value] -> IO ())]
inExecTable = [("cd"     , lcd)
              ,("echo"   ,lecho)
              ,("exit"   ,lexit)
              ,("pwd"    ,lpwd)
              ,("lambda" ,lambda)
              ,("help"   ,lhelp)]

lcd :: [Value] -> IO ()
lcd v  = do
  setCurrentDirectory $ show $ v !! 0

lpwd :: [Value] -> IO ()
lpwd _ = do
  x <- getCurrentDirectory
  putStrLn x

lhelp :: [Value] -> IO ()
lhelp _ = do
  putStrLn H.help

lecho :: [Value] -> IO ()
lecho w = do
  if ((String "-n") `elem` w) then do
    putStr $ (concat . filter (/="-n") . map show) w
    else do
    putStrLn $ (concat . map show) w

lambda :: [Value] -> IO ()
lambda _ = do
  putStrLn H.lambda

lexit :: [Value] -> IO ()
lexit _ = exitWith $ ExitSuccess

sh :: Statement -> IO ()
sh (Command cmd args)= do
  let action = lookup cmd inExecTable
  case action of
    (Just exec) ->  exec args
    Nothing     -> do
      (cod, out, err) <- readProcessWithExitCode cmd (map show args) ""
      putStrLn $ out

sh (Val (String cmd))= do
  let action = lookup cmd inExecTable
  case action of
    (Just exec) ->  exec []
    Nothing     -> do
      (cod, out, err) <-  readProcessWithExitCode cmd [] ""
      putStrLn $ out 

-- a Store holding the environment
data Store = Store (Map String String) deriving (Eq, Show)

-- Empty store
empty :: Store
empty = Store Map.empty

-- lookup variables in the store
slookup :: String -> Store -> String
slookup x (Store m) = case (Map.lookup x m) of
    Just v ->  v
    Nothing -> "" -- change this?

-- update the value of a variable in the store
update :: String -> String -> Store -> Store
update x v (Store m) = Store (Map.insert x v m)


arglist = [ "-c"  -- non-interactive
          , "-v"] -- verbose

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> repl empty
    1 -> putStrLn "Reading from file not supported yet!"
    2 -> if ( args !! 0 ) `elem` arglist then
           eval (args !! 1)
         else
           putStrLn $ "Unknown arg" ++ (show (args !! 0))
    otherwise -> putStrLn "Only 0-2 arguments!"

repl :: Store -> IO ()
repl store = do
  putStr "λ> "
  hFlush stdout
  line <- getLine
  eval line
  repl store
--  args <- getArgs

eval :: String -> IO ()
eval input =  case (parse parseStat "Shell Statement" (input)) of
  Left err -> do
    putStrLn $ "No match" ++ show err
  Right v  -> do
    sh v

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

-- Pretty printing for the Shell language
