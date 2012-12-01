module Evaluator where

import System.IO
import qualified Help as H
import System.Cmd
import System.Process
import System.Exit
import System.Directory
import Control.Monad
import Types
import Data.Map (Map)
import qualified Data.Map as Map


inExecTable :: [(String, [Value] -> Uni-> IO ())]
inExecTable = [("cd"     , lcd)
              ,("echo"   ,lecho)
              ,("exit"   ,lexit)
              ,("quit"   ,lexit)
              ,("set"    ,lset)
              ,("pwd"    ,lpwd)
              ,("lambda" ,lambda)
              ,("history",lhist)
              ,("help"   ,lhelp)]

lcd :: [Value] -> Uni -> IO ()
lcd v _ = do
  setCurrentDirectory $ show $ v !! 0

lpwd :: [Value] -> Uni -> IO ()
lpwd _ _ = do
  x <- getCurrentDirectory
  pp $ x ++ "\n"

lhelp :: [Value] -> Uni -> IO ()
lhelp _ _ = do
  pp H.help

lecho :: [Value] -> Uni -> IO ()
lecho w _ = do
  if ((String "-n") `elem` w) then do
    pp $ (concat . filter (/="-n") . map show) w
    else do
    putStrLn  $ (concat . map show)  w

lambda :: [Value] -> Uni -> IO ()
lambda _ _ = do
  pp H.lambda

-- TODO: print history values
lhist :: [Value] -> Uni -> IO ()
lhist args uni = do
  case (length args) of
    0 -> pp $ expose (toInteger $ length (history uni) - 10) $ history uni
    1 -> case (args !! 0) of
      Number n -> pp $ expose n $ history uni
      _ -> putStrLn "history: strings not supported"
           -- we need real state to utilize "-c"
    _ -> putStrLn "Wrong number of arguments"

  where expose :: Integer -> [String]-> String
        expose n xs= unlines $ drop (fromIntegral (n - 1))
                     $ zipWith (\ x y -> x ++ "\t" ++ y)
                     (map show [1..]) xs

-- TODO change history to non-local
lexit :: [Value] -> Uni -> IO ()
lexit _ uni = do
  -- h <- getHomeDirectory
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle $ unlines $ history uni
  hClose tempHandle
  renameFile tempName ".lsh_history"
  exitWith $ ExitSuccess

-- TODO use Uni
lset :: [Value] -> Uni -> IO ()
lset x _ = do
  putStrLn $ show x

sh :: Statement -> Uni -> Map Variable Value -> IO (Map Variable Value)
sh (Command cmd args) uni store = do
  let action = lookup cmd inExecTable
  case action of
    (Just exec) -> do
                   exec args uni
                   return store
    Nothing     -> do
      (cod, out, err) <- readProcessWithExitCode cmd (map show args) ""
      pp $ out
      return store

sh (Val (String cmd)) uni store = do
  let action = lookup cmd inExecTable
  case action of
    (Just exec) -> do
                   exec [] uni
                   return store
    Nothing     -> do
      (cod, out, err) <-  readProcessWithExitCode cmd [] ""
      pp $ out
      return store

sh v@(Val _) uni store = do 
  pp $ show v
  return store

-- TODO Mike, can you add a store here? I will take care of output later
sh v@(Assign var val) uni store = do
  pp $ show v
  return (Map.insert var val store)

pp :: String -> IO ()
pp str = putStr str >> hFlush stdout