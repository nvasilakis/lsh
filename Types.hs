module Types where

-- Syntax of Shell language

type Variable = String
type Args = [Value]

data Statement =
    Command String Args       -- echo "a b"
  | Val Value                 -- 3 or string or "quoted String"
  | Assign Variable Value     -- Assign x 3, Assign, x "quoted String"
--  deriving Show
{-
data Expression =
    Var Variable                    -- x
  | Val Value                       -- v
-}

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

