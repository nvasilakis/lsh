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


inExecTable :: [(String, [Value] -> Out -> Uni-> IO (Uni))]
inExecTable = [("cd"     , lcd)
              ,("echo"   ,lecho)
              ,("exit"   ,lexit)
              ,("quit"   ,lexit)
              ,("set"    ,lset)
              ,("pwd"    ,lpwd)
              ,("lambda" ,lambda)
              ,("history",lhist)
              ,("help"   ,lhelp)]

lcd, lpwd, lhelp, lecho, lambda, lhist :: [Value] -> Out -> Uni -> IO (Uni)
lset, lexit :: [Value] -> Out -> Uni -> IO (Uni)

lcd v _ u = do
  setCurrentDirectory $ show $ v !! 0
  return u

--lpwd :: [Value] -> Uni -> IO (Uni)
lpwd _ o u = do
  x <- getCurrentDirectory
  y <- pp o $ x ++ "\n"
  return $ resolve u y

lhelp _ o u = do
  x <- pp o H.help
  return $ resolve u x
-- TODO: Check for $ in the incoming string
lecho w o u = do
  x <- if ((String "-n") `elem` w) then do
    pp o $ (concat . filter (/="-n") . map show) w
    else do
    pp o (((concat . map show)  w ) ++ "\n")
  return $ resolve u x

lambda v o u = do
  x <- pp o H.lambda
  return $ resolve u x

-- TODO: Manipulate history (clear)
lhist args o uni = do
  x <- case (length args) of
    0 -> pp o $ expose (toInteger $ length (history uni) - 10) $ history uni
    1 -> case (args !! 0) of
      Number n -> pp o $ expose n $ history uni
      _ -> pp Screen "history: strings not supported\n"
           -- we need real state to utilize "-c"
    _ -> pp Screen "Wrong number of arguments\n"
  return $ resolve uni x

  where expose :: Integer -> [String]-> String
        expose n xs= unlines $ drop (fromIntegral (n - 1))
                     $ zipWith (\ x y -> x ++ "\t" ++ y)
                     (map show [1..]) xs
-- TODO: Grab history to home dir or grab from resources
lexit _ o u = do
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle $ unlines $ history u
  hClose tempHandle
  renameFile tempName ".lsh_history"
  exitWith $ ExitSuccess

-- TODO use Uni
lset args o u =
  case (length args) of
    1 -> return $ updateConfiguration u $ Map.insert (show (args !! 0))
         ("True") $ configuration u -- Add boolean to Value?
    2 -> return $ updateConfiguration u $ Map.insert (show ( args !! 0))
         (show (args !! 1)) $ configuration u
    _ -> return u

------------------------ Evaluation function
sh :: Complex -> Uni  -> IO (Uni)
sh (Pipe c1 c2) uni = do
  uni2 <- sh c1 uni -- We will need to print nothing here and push input fwd!
  uniLast <- sh c2 uni2
  return uniLast

sh (Semi c1) uni = do
  uni2 <- sh c1 uni
  return uni2

sh (Noop) uni = do -- TODO: what if in middle of pipeline
  pp Screen ""
  return uni

sh (Higher Map c1 c2) uni = undefined

sh (Higher Fold c1 c2) uni = undefined

sh (Higher Filter c1 c2) uni = undefined

sh (Higher ZipWith c1 c2) uni = undefined


sh (Statement ( Command cmd args)) uni = do
  let action = lookup cmd inExecTable
  case action of
    (Just exec) -> do
                   exec args Screen uni
                   return uni
    Nothing     -> do
      (cod, out, err) <- readProcessWithExitCode cmd (map show args) ""
      pp Screen $ out
      return uni

sh (Statement (Val (String cmd))) uni = do
  let action = lookup cmd inExecTable
  case action of
    (Just exec) -> do
                   exec [] Screen uni
                   return uni
    Nothing     -> do
      (cod, out, err) <-  readProcessWithExitCode cmd [] ""
      pp Screen $ out
      return uni

sh v@(Statement (Val _)) uni = do
  pp Screen $ show v -- TODO: needed?
  return uni

sh v@(Statement (Assign var val)) uni = do
  return $ updateVars uni (Map.insert var val $ variables uni)

--------------- helpers

pp :: Out -> String -> IO (Maybe String)
pp o str = case o of
  Screen -> do
    putStr str
    hFlush stdout
    return (Nothing)
  Redirect -> do
    return (Just str)
-- TODO: This should take a [String] as a second arg
resolve :: Uni -> Maybe String -> Uni
resolve u s = case s of
  (Just y) -> (updateHistory u [y])
  Nothing  -> u
