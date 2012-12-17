module Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ (Doc, (<+>),(<>))
import qualified Text.PrettyPrint.HughesPJ as PP

-- Syntax of Shell language

type Variable = String
type Args = [Value]
type Hist = [String]
type Config = Map.Map String String
type Vars = Map.Map Variable Value
type AliasStore = Map.Map Variable Value
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
                         , output :: [String]
                         , exitCode :: Int
                         , alias :: AliasStore }
-- we can definitely name it `Un` though~!
type Uni = Universe

data Complete =
  Complex Complex                   -- complex method types
  | Semi Complete                   -- echo "a b"; method ends in semi
  | Ssep Complete Complete          -- echo "a b"; echo "c" seperated by semi
  deriving (Eq)

-- 4 examples of more elaborate scripting
data Complex =
    Pipe Complex Complex            -- echo "a b" | grep 'a'
--  | Semi Complex                    -- echo "a b"; -- echo "a b"; echo "c"
  | Higher Higher Complex Complex   -- map/fold/filter/zipWith
  | Statement Statement             -- echo "a b"
  | Alias Args
  | Noop
  deriving (Eq)

data Higher =
    Map
  | Fold
  | Filter
  | ZipWith
  deriving (Eq)

data Out =  Screen | Redirect

-- 4 basic structures
-- TODO: add alias, if, while
data Statement =
    Command String Args       -- echo "a b"
  | Val Value                 -- 3 or string or "quoted String"
  | Assign Variable Value     -- Assign x 3, Assign, x "quoted String"
  deriving (Eq)

-- literals
-- TODO: add single-quoted string, boolean?
data Value =
    Number Integer -- 3
  | String String  -- abcd
  | Quoted String  -- "ab cde"
    deriving (Eq)

showStatement :: Statement -> String
showStatement (Command cmd args) = cmd ++ " : " ++ show args
showStatement (Val val) = show val
showStatement (Assign var val) = (show var) ++ " = " ++ (show val)
instance Show Statement where show = showStatement

showValue (String x) = x
showValue (Number x) = show x
showValue (Quoted x) = x
instance Show Value where show = showValue

showHigher (Map) = " map"
showHigher (Fold) = " fold"
showHigher (Filter) = " filter"
showHigher (ZipWith) = " zipWith"
instance Show Higher where show = showHigher

showComplex :: Complex -> String
showComplex (Pipe a b) = show a ++ " | " ++ show b
showComplex (Higher s a b) = show s ++ " {" ++ show a ++ "} {" ++ show b ++ "}"
showComplex (Statement a) = show a
showComplex (Noop) = show "\n"
showComplex (Alias a) = "alias " ++ show a
instance Show Complex where show = showComplex
                            
showComplete :: Complete -> String
showComplete (Complex c) = show c
showComplete (Semi a) = show a ++ ";"
showComplete (Ssep a b) = show a ++ " ; " ++ show b
instance Show Complete where show = showComplete

-- Specific functions for the record; I guess the only advantage
-- of this approach is to leverage benefits from the typesystem
updateHistory :: Uni -> Hist -> Uni
updateHistory u h = Universe
                    (h)
                    (configuration u)
                    (variables u)
                    (output u)
                    (exitCode u)
                    (alias u)

-- updateConfiguration uc c = uc {configuration = c }
updateConfiguration :: Uni -> Config -> Uni
updateConfiguration u c = Universe
                          (history u)
                          (c)
                          (variables u)
                          (output u)
                          (exitCode u)
                          (alias u)

updateVars :: Uni -> Vars -> Uni
updateVars u v = Universe
                   (history u)
                   (configuration u)
                   (v)
                   (output u)
                   (exitCode u)
                   (alias u)

updateOutput :: Uni -> [String] -> Uni
updateOutput u o = Universe
                   (history u)
                   (configuration u)
                   (variables u)
                   (o)
                   (exitCode u)
                   (alias u)

updateExitCode :: Uni -> Int -> Uni
updateExitCode u e = Universe
                   (history u)
                   (configuration u)
                   (variables u)
                   (output u)
                   (e)
                   (alias u)
                   
updateAlias :: Uni -> AliasStore -> Uni
updateAlias u a = Universe
                   (history u)
                   (configuration u)
                   (variables u)
                   (output u)
                   (exitCode u)
                   (a)
                   
defaultUni :: Uni
defaultUni = Universe [] Map.empty Map.empty [] 0 Map.empty

-- for testing and alias
class PP a where
  pprint :: a -> Doc
  
instance PP Value where
  pprint (Number x) = PP.integer x
  pprint (String x) = PP.text x
  pprint (Quoted x) = PP.doubleQuotes $ PP.text x
  
instance PP Statement where
  pprint (Command str args) = PP.text str <+> PP.hsep (map pprint args)
  pprint (Val v) = pprint v
  pprint (Assign var val) = PP.text var <> PP.equals <> pprint val
  
instance PP Higher where
  pprint Map = PP.text "map"
  pprint Fold = PP.text "fold"
  pprint Filter = PP.text "filter"
  pprint ZipWith = PP.text "zipWith"
  
instance PP Complex where
  pprint (Pipe cpl1 cpl2) = pprint cpl1 <+> PP.text "|" <+> pprint cpl2
  pprint (Higher hfunc cpl1 cpl2) = pprint hfunc 
                                <+> 
                                PP.lparen <> pprint cpl1 <> PP.rparen
                                <+> 
                                PP.lparen <> pprint cpl2 <> PP.rparen
  pprint (Statement st) = pprint st
  pprint (Noop) = PP.text "\n"
  pprint (Alias args) = PP.text "alias" <+> PP.hsep (map pprint args)
  
instance PP Complete where
  pprint (Complex x) = pprint x
  pprint (Semi c) = pprint c <> PP.semi
  pprint (Ssep c1 c2) = pprint c1 <+> PP.semi <+> pprint c2

display :: PP a => a -> String
display = show . pprint
