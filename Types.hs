module Types where

-- Syntax of Shell language

type Variable = String
type Args = [Value]

-- 4 examples of more elaborate scripting
data Complex = 
    Pipe Complex Complex            -- echo "a b" | grep 'a'
  | Semi Complex                    -- echo "a b"; -- echo "a b"; echo "c"
  | Higher String Complex Complex   -- map/fold/filter/zipWith
  | Statement Statement             -- echo "a b"

-- 4 basic structures
-- TODO: add alias, if, while
data Statement =
    Command String Args       -- echo "a b"
  | Val Value                 -- 3 or string or "quoted String"
  | Assign Variable Value     -- Assign x 3, Assign, x "quoted String"

-- literals
-- TODO: add single-quoted string
data Value =
    Number Integer -- 3
  | String String  -- abcd
  | Quoted String  -- "ab cde"
    deriving (Eq)

showStatement :: Statement -> String
showStatement (Command cmd args) =  "Running " ++ cmd ++ " | " ++ show args
showStatement (Val val) = show val
showStatement (Assign var val) = (show var) ++ " = " ++ (show val)
instance Show Statement where show = showStatement

showValue (String x) =  x
showValue (Number x) = show x
showValue (Quoted x) = show x
instance Show Value where show = showValue

showComplex :: Complex -> String
showComplex (Pipe a b) = show a ++ " | " ++ show b
showComplex (Semi a) = show a ++ ";"
showComplex (Higher s a b) = show s ++ " {" ++ show a ++ "} {" ++ show b ++ "}"
showComplex (Statement a) = show a 
instance Show Complex where show = showComplex
