module Evaluator where

import System.IO
import qualified Help as H
import System.Cmd
import System.Process
import System.Exit
import System.Directory
import Control.Monad
import Types

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
