module Main where

import System.Environment
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import Text.ParserCombinators.Parsec
import Parser
import Evaluator

--import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
--import qualified Text.PrettyPrint.HughesPJ as PP

import Control.Monad.Error
import Control.Monad.State

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


-- Pretty printing for the Shell language
