module Main where

import System.Environment
import System.Directory
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import Text.ParserCombinators.Parsec
import Parser
import Evaluator
import ConfigFile

--import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
--import qualified Text.PrettyPrint.HughesPJ as PP

import Control.Monad.Error
import Control.Monad.State

-- a Store holding the environment
data Store = Store (Map String String) deriving (Eq, Show)

type Hist = [String]

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
  conf <- lshInit
  case length args of
    0 -> repl empty conf
    1 -> putStrLn "Reading from file not supported yet!"
    2 -> if ( args !! 0 ) `elem` arglist then
           eval (args !! 1)
         else
           putStrLn $ "Unknown arg" ++ (show (args !! 0))
    otherwise -> putStrLn "Only 0-2 arguments!"

repl :: Store  -> Config -> IO ()
repl store conf = do
  putStr $ ps1 conf
  hFlush stdout
  line <- getLine
  eval line
  repl store conf
--  args <- getArgs

ps1 :: Config -> String
ps1 conf = case (Map.lookup "prompt" conf) of
  Just x -> x
  Nothing -> "λ> "

-- TODO: Should this be in Evaluator module?
eval :: String -> IO ()
eval input =  case (parse parseHandler "Shell Statement" (input)) of
  Left err -> do
    putStrLn $ "No match" ++ show err
  Right v  -> do
    sh v

{-
parseInit :: String -> IO (Config)
parseInit init = case (parseFromFile file init) of
    Left err -> return $ Map.empty
    Right xs -> return $ Map.fromList (reverse xs)
-}

-- TODO: Add home into the config
-- looks first locally and then in home
lshInit :: IO (Config)
lshInit = do
  x <- doesFileExist ".lshrc"
  case x of
    False -> do
      h <- getHomeDirectory
      readConfig (h ++ ".lshrc")
    True -> readConfig ".lshrc"

-- TODO: Check if user changed his histfile
-- TODO: We could probably move Hist to Text
lshHist :: IO (Hist)
lshHist = do
--  h <- getHomeDirectory
  let h = "./"
  x <- doesFileExist ( h ++ ".lsh_history")
  case x of
    True  -> do
      handle <- openFile (h ++ ".lsh_history") ReadMode
      contents <- hGetContents handle
      hClose handle
      return $ lines contents
    False -> do
      return []