module Types where

import qualified Data.Map as Map

-- Syntax of Shell language

type Variable = String
type Args = [Value]
type Hist = [String]
type Config = Map.Map String String
type Vars = Map.Map Variable Value
{-
TODO: We need to think about how to represent default values
(Monad? Typeclass that has default? I think it is a monad because
 it can capture two values (a la Error), either found or not. We
can pattern match on maybe till then)
-}

-- Env would be better, but it is already semantically
-- "taken" in the context of a shell (man env)!
data Universe = Universe { history :: Hist
                         , configuration :: Config
                         , variables :: Vars
                         , output :: [String]}
-- we can definitely name it `Un` though~!
type Uni = Universe

-- Specific functions for the record; I guess the only advantage
-- of this approach is to leverage benefits from the typesystem
updateHistory :: Uni -> Hist -> Uni
updateHistory u h = Universe
                    (h)
                    (configuration u)
                    (variables u)
                    (output u)
updateConfiguration :: Uni -> Config -> Uni
updateConfiguration u c = Universe
                          (history u)
                          (c)
                          (variables u)
                          (output u)
updateVars :: Uni -> Vars -> Uni
updateVars u v = Universe
                   (history u)
                   (configuration u)
                   (v)
                   (output u)

updateOutput :: Uni -> [String] -> Uni
updateOutput u o = Universe
                   (history u)
                   (configuration u)
                   (variables u)
                   (o)

-- 4 examples of more elaborate scripting
data Complex =
    Pipe Complex Complex            -- echo "a b" | grep 'a'
  | Semi Complex                    -- echo "a b"; -- echo "a b"; echo "c"
  | Higher Higher Complex Complex   -- map/fold/filter/zipWith
  | Statement Statement             -- echo "a b"

data Higher =
    Map
  | Fold
  | Filter
  | ZipWith

data Out =  Screen | Redirect

-- 4 basic structures
-- TODO: add alias, if, while
data Statement =
    Command String Args       -- echo "a b"
  | Val Value                 -- 3 or string or "quoted String"
  | Assign Variable Value     -- Assign x 3, Assign, x "quoted String"

-- literals
-- TODO: add single-quoted string, boolean?
data Value =
    Number Integer -- 3
  | String String  -- abcd
  | Quoted String  -- "ab cde"
    deriving (Eq)

showStatement :: Statement -> String
showStatement (Command cmd args) =  " " ++ cmd ++ " : " ++ show args
showStatement (Val val) = show val
showStatement (Assign var val) = (show var) ++ " = " ++ (show val)
instance Show Statement where show = showStatement

showValue (String x) =  x
showValue (Number x) = show x
showValue (Quoted x) = show x
instance Show Value where show = showValue

showHigher (Map) = "map"
showHigher (Fold) = "fold"
showHigher (Filter) = "filter"
showHigher (ZipWith) = "zipWith"
instance Show Higher where show = showHigher

showComplex :: Complex -> String
showComplex (Pipe a b) = show a ++ " | " ++ show b
showComplex (Semi a) = show a ++ ";"
showComplex (Higher s a b) = show s ++ " {" ++ show a ++ "} {" ++ show b ++ "}"
showComplex (Statement a) = show a
instance Show Complex where show = showComplex
