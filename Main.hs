module Main where

import System.Environment
import System.Directory
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import Text.ParserCombinators.Parsec
import Types -- try to remove this one!
import Parser
import Evaluator
import ConfigFile

--import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
--import qualified Text.PrettyPrint.HughesPJ as PP

import Control.Monad.Error
import Control.Monad.State

arglist = [ "-c"  -- non-interactive
          , "-v"] -- verbose

testStore :: Map Variable Value
testStore = Map.empty

main :: IO ()
main = do
  let vars = Map.empty
      alias = Map.empty
  args <- getArgs
  conf <- lshInit
  hist <- lshHist -- TODO: this needs to be put lower for non interactive
  let u = (Universe hist conf vars [""] 0 alias)
  case length args of
    0 -> repl u
    1 -> putStrLn $ simple args u
    2 -> if ( args !! 0 ) `elem` arglist then do
           eval (args !! 1) u
           return ()
         else
           putStrLn $ "Unknown arg" ++ (show (args !! 0))
    otherwise -> putStrLn "Only 0-2 arguments!"

repl :: Uni  -> IO ()
repl uni = do
  putStr $ ps1 $ configuration uni
  hFlush stdout
  line <- getLine
  newUni <- eval line uni
  repl (appendToHistory line newUni)

-- TODO: Append normally, and reverse when read
-- It also clears the previous output
appendToHistory :: String -> Uni -> Uni
appendToHistory line uni =
  if (length line > 0) then
    Universe
    ((history uni) ++ [line])
    (configuration uni)
    (variables uni)
    ([""])
    0
    (alias uni)
    else uni

simple :: [String] -> Uni -> String -- eliminating new line
simple args uni = case (args !! 0) of
  "-f" -> "No parse file functionality"
  _    -> init . unlines $ history uni

ps1 :: Config -> String
ps1 conf = case (Map.lookup "prompt" conf) of
  Just x -> x
  Nothing -> "λ> "

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

-- TODO: Check if user changed location of his histfile
-- TODO: We could probably move Hist to Text
lshHist :: IO (Hist)
lshHist = do
--  h <- getHomeDirectory
--  let h = "./"
  x <- doesFileExist ( ".lsh_history")
  case x of
    True  -> do
      text <- readFile (".lsh_history")
      --contents <- hGetContents handle
      --putStrLn contents
      --hClose handle
      return $ lines text
    False -> do
      return []