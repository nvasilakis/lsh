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
              ,("quit"   ,lexit)
              ,("set"    ,lset)
              ,("pwd"    ,lpwd)
              ,("lambda" ,lambda)
              ,("history",lhist)
              ,("help"   ,lhelp)]

lcd :: [Value] -> IO ()
lcd v  = do
  setCurrentDirectory $ show $ v !! 0

lpwd :: [Value] -> IO ()
lpwd _ = do
  x <- getCurrentDirectory
  pp $ x ++ "\n"

lhelp :: [Value] -> IO ()
lhelp _ = do
  pp H.help

lecho :: [Value] -> IO ()
lecho w = do
  if ((String "-n") `elem` w) then do
    pp $ (concat . filter (/="-n") . map show) w
    else do
    putStrLn  $ (concat . map show)  w

lambda :: [Value] -> IO ()
lambda _ = do
  pp H.lambda

-- TODO: print history values
lhist :: [Value] -> IO ()
lhist _ = do
  putStrLn "undefined"

lexit :: [Value] -> IO ()
lexit _ = exitWith $ ExitSuccess

lset :: [Value] -> IO ()
lset x = do
  putStrLn $ show x

sh :: Statement -> IO ()
sh (Command cmd args)= do
  let action = lookup cmd inExecTable
  case action of
    (Just exec) ->  exec args
    Nothing     -> do
      (cod, out, err) <- readProcessWithExitCode cmd (map show args) ""
      pp $ out

sh (Val (String cmd))= do
  let action = lookup cmd inExecTable
  case action of
    (Just exec) ->  exec []
    Nothing     -> do
      (cod, out, err) <-  readProcessWithExitCode cmd [] ""
      pp $ out

pp :: String -> IO ()
pp str = putStr str >> hFlush stdout
