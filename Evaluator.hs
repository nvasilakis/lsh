module Evaluator where

import System.IO
import qualified Help as H
import System.Cmd
import System.Process
import System.Exit
import System.Directory
import Control.Monad
import Types

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
    0 -> putStrLn $ unlines $ history uni
    1 -> case (args !! 0) of
      Number n -> putStrLn $ expose n $ history uni
      _ -> putStrLn "history: strings not supported"
    _ -> putStrLn "Wrong number of arguments"

  where expose :: Integer -> [String]-> String
        expose n xs= unlines $ zipWith (\ x y -> x ++ "\t\t" ++ y)
                   (map show [1..]) (reverse $ take (fromIntegral n) $ reverse xs)

-- TODO change history to non-local
lexit :: [Value] -> Uni -> IO ()
lexit _ _ = do
  -- h <- getHomeDirectory
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle $ unlines ["this", "isn't", "history"]
  hClose tempHandle
  renameFile tempName ".lsh_history"
  exitWith $ ExitSuccess

-- TODO use Uni
lset :: [Value] -> Uni -> IO ()
lset x _ = do
  putStrLn $ show x

sh :: Statement -> Uni -> IO ()
sh (Command cmd args) uni = do
  let action = lookup cmd inExecTable
  case action of
    (Just exec) ->  exec args uni
    Nothing     -> do
      (cod, out, err) <- readProcessWithExitCode cmd (map show args) ""
      pp $ out

sh (Val (String cmd)) uni = do
  let action = lookup cmd inExecTable
  case action of
    (Just exec) ->  exec [] uni
    Nothing     -> do
      (cod, out, err) <-  readProcessWithExitCode cmd [] ""
      pp $ out

pp :: String -> IO ()
pp str = putStr str >> hFlush stdout
