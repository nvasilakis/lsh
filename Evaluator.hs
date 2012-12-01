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

sh v@(Val _) uni = do pp $ show v

-- TODO Mike, can you add a store here? I will take care of output later
sh v@(Assign var val) uni = do pp $ show v


pp :: String -> IO ()
pp str = putStr str >> hFlush stdout
