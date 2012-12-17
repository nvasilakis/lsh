module Evaluator where

import System.IO
import qualified Help as H
import System.Cmd
import System.Process
import System.Exit
import System.Directory
import Control.Monad
import Types
import Parser
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Text.ParserCombinators.Parsec

inExecTable :: [(String, [Value] -> Out -> Uni-> IO (Uni))]
inExecTable = [("cd"     ,lcd)
              ,("echo"   ,lecho)
              ,("exit"   ,lexit)
              ,("quit"   ,lexit)
              ,("set"    ,lset)
              ,("pwd"    ,lpwd)
              ,("lambda" ,lambda)
              ,("history",lhist)
              ,("help"   ,lhelp)
              ,("alias"  ,lalias)]

lcd, lpwd, lhelp, lecho, lambda, lhist :: [Value] -> Out -> Uni -> IO (Uni)
lset, lexit, lalias :: [Value] -> Out -> Uni -> IO (Uni)

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
  let nw = map (replaceVariable u) w
  x <- if ((String "-n") `elem` nw) then do
    pp o $ (concat . filter (/="-n") . map show) nw
    else do
    pp o (((concat . map show)  nw ) ++ "\n")
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

-- TODO this is just an simple alias implementation
lalias args _ u =
  case (length args) of
    0 -> do
         putStrLn $ show $ alias u
         return u -- TODO print all the alias
    _ -> do
         let input = intercalate " " (map show args)
         case (parse (many parseAliasArgs) "" input) of
           Left err -> return u
           Right v  -> return $ insertList v u 
                         where insertList [] uni = uni
                               insertList ((var,val):xs) uni = 
                                 insertList xs $ updateAlias uni 
                                 (Map.insert var val $ alias uni)

------------------------ Evaluation function
sh :: Complex -> Out -> Uni  -> IO (Uni)
sh (Pipe c1 c2) out uni = do
  -- We will need to print nothing here and push input fwd!
  uni2 <- sh c1 Redirect uni
  uniLast <- sh c2 Screen uni2
  return uniLast

--sh (Semi c1) out uni = sh c1 Screen uni
  
sh (Alias args) out uni = lalias args out uni

sh (Noop) out uni = do -- TODO: what if in middle of pipeline
  pp Screen ""
  return uni

sh (Higher Map c1 c2) out uni = do
  uni2 <- sh c2 Redirect uni
  results <- lmap c1 $ output uni2
  pp out $ unlines results
  return $ updateOutput uni2 results
  where lmap :: Complex -> [String] -> IO ([String])
        lmap c (x:xs) = do
          putStrLn $ "heyhey " ++ x
          u <- sh c Redirect $ updateOutput defaultUni [x]
          liftM2 (++) (return $ output u) (lmap c xs)
        lmap _ [] = return ([])

sh (Higher Fold c1 c2) out uni = undefined

sh (Higher Filter c1 c2) out uni = do
  uni2 <- sh c2 Redirect uni
  results <- lfilter c1 $ output uni2
  pp out $ unlines results
  return $ updateOutput uni2 results
  where lfilter :: Complex -> [String] -> IO ([String])
        lfilter c (x:xs) = do  -- create a fake uni
          u <- sh c Redirect $ updateOutput defaultUni [x]
          case (exitCode u) of
            0 -> liftM2 (:) (return (x)) (lfilter c xs)
            _ -> lfilter c xs
        lfilter _ [] = return ([])

sh (Higher ZipWith c1 c2) out uni = undefined

-- replace the cmd with the string in alias
-- redo the parsing and redo eval
sh st@(Statement (Command cmd args)) out uni =
  case (Map.lookup cmd $ alias uni) of -- check alias
    (Just (Quoted s)) -> do
      newUni <- eval (s ++ " " ++ (intercalate " " (map show args))) uni
      return newUni
    _                 -> do
      let action = lookup cmd inExecTable
      case action of
        (Just exec) -> do
          newUni <- exec args out uni
          return newUni
        Nothing     -> do
          (cod, stOut, stErr) <- readProcessWithExitCode cmd 
                                 (map show args) $ intercalate "\n" $ output uni
          let e = case cod of
                ExitSuccess -> 0
                ExitFailure z -> z
          c <- pp out $ stOut
          return $ resolve (updateExitCode uni e) c

-- alias in this case is simple 
-- match the alias and redo eval
sh (Statement (Val (String cmd))) out uni = do
  case (Map.lookup cmd $ alias uni) of -- check alias
    (Just (Quoted s)) -> do
      newUni <- eval s uni
      return newUni
    _                 -> do
      let action = lookup cmd inExecTable
      case action of
        (Just exec) -> do
          newUni <- exec [] out uni
          return newUni
        Nothing     -> do
          (cod, stOut, stErr) <- readProcessWithExitCode cmd
                                 [] $ intercalate "\n" $ output uni
          let e = case cod of
                ExitSuccess -> 0
                ExitFailure z -> z
          c <- pp out $ stOut
          return $ resolve (updateExitCode uni e) c

sh v@(Statement (Val _)) out uni = do
  pp Screen $ show v -- TODO: needed?
  return uni

sh v@(Statement (Assign var val)) out uni = do
  return $ updateVars uni (Map.insert var val $ variables uni)

eval :: String -> Uni -> IO (Uni)
eval input uni = case (parse parseComplex "Shell Statement" (input)) of
  Left err -> do
    putStrLn $ "No match" ++ show err
    return uni
  Right v  -> do
    sh v Screen uni

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
  (Just y) -> (updateOutput u $ lines y)
  Nothing  -> u

replaceVariable :: Uni -> Value -> Value
replaceVariable _   (Number a) = Number a
replaceVariable uni (String a) = String $ replaceDollar uni a
replaceVariable uni (Quoted a) = Quoted $ replaceDollar uni a