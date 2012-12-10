module Tests where

import Types
import Parser

import Test.HUnit
import Text.ParserCombinators.Parsec
import Test.QuickCheck
import Text.PrettyPrint.HughesPJ (Doc, (<+>),(<>))
import qualified Text.PrettyPrint.HughesPJ as PP
import Control.Monad

--quickcheck start  

--printing function for types

class PP a where
  pp :: a -> Doc
  
instance PP Value where
  pp (Number x) = PP.integer x
  pp (String x) = PP.text x
  pp (Quoted x) = PP.doubleQuotes $ PP.text x
  
instance PP Statement where
  pp (Command str args) = PP.text str <+> PP.hsep (map pp args)
  pp (Val v) = pp v
  pp (Assign var val) = PP.text var <> PP.equals <> pp val
  
instance PP Higher where
  pp Map = PP.text "map"
  pp Fold = PP.text "fold"
  pp Filter = PP.text "filter"
  pp ZipWith = PP.text "zipWith"
  
instance PP Complex where
  pp (Pipe cpl1 cpl2) = pp cpl1 <+> PP.text "|" <+> pp cpl2
  pp (Semi cpl) = pp cpl <> PP.semi
  pp (Higher hfunc cpl1 cpl2) = pp hfunc 
                                <+> 
                                PP.lparen <> pp cpl1 <> PP.rparen
                                <+> 
                                PP.lparen <> pp cpl2 <> PP.rparen
  pp (Statement st) = pp st
  pp (Noop) = PP.text "\n"
  
display :: PP a => a -> String
display = show . pp
  
--generate random statements

instance Arbitrary Value where
  arbitrary = oneof [elements [Number 3,Number 232],
                     -- keep number > 0
                     elements [String "ab",String "cd"],
                     elements [Quoted "ef d", Quoted "gh"]]
  
  shrink _ = []
              
instance Arbitrary Statement where
  arbitrary = oneof [elements [Command "echo" [String "ab"]],
                     elements [Val (Number 314),
                               Val (String "ls")],
                     liftM (Assign "b") arbitrary]
  
  shrink _ = []

instance Arbitrary Higher where
  arbitrary = elements [Map,Fold,Filter,ZipWith]

--if do not add frequency here some test case cannot finish generating
instance Arbitrary Complex where
  arbitrary = frequency [(4, astate),
                         (2, apipe),
                         (1, ahigher),
                         (1, asemi)
                        ] 
    where astate = liftM Statement arbitrary
          asemi = liftM Semi arbitrary
          apipe = oneof [liftM2 Pipe arbitrary astate,
                         liftM2 Pipe arbitrary ahigher]
          ahigher = oneof [liftM3 Higher arbitrary arbitrary arbitrary]
  
  shrink _ = []

--quickCheck properties

checkComplexParser st = 
  case (parse parseComplex "Shell Parser" (display st)) of
    Left  _ -> False
    Right a -> st == a
  where types = st :: Complex

main :: IO ()
main = do
  verboseCheck checkComplexParser
  --quickCheck checkComplexParser
  
--quickcheck end  
  
runStaticTests :: IO Counts
runStaticTests = runTestTT $ TestList testList

testList :: [Test]
testList = [ tStatementVal,
             tStatementAssign,
             tStatementCommand ]

parseStatementTest :: String -> Either ParseError Complex
parseStatementTest str = parse parseStatement "Shell Statement" str

succeed :: (Eq a) => Either ParseError a -> a -> Assertion
succeed (Left _) _ = assert False
succeed (Right r) rt = if r == rt
                       then assert True
                       else assert False

failure :: Either ParseError a -> Assertion
failure (Left _) = assert True
failure (Right _) = assert False

tStatementVal :: Test
tStatementVal =
  "Test Statement Val" ~:
  TestList [
    "sv1" ~: succeed
    (parseStatementTest "-1")
    (Statement (Val (String "-1"))),
    "sv2" ~: succeed
    (parseStatementTest "21")
    (Statement (Val (Number (21)))),
    "sv3" ~: succeed
    (parseStatementTest "   1")
    (Statement (Val (Number (1)))),
    "sv4" ~: succeed
    (parseStatementTest "   1    ")
    (Statement (Val (Number (1)))),
    "sv5" ~: succeed
    (parseStatementTest "1    ")
    (Statement (Val (Number (1)))),
    "sv6" ~: succeed
    (parseStatementTest "abc")
    (Statement (Val (String "abc"))),
    "sv7" ~: succeed
    (parseStatementTest "\"abc\"")
    (Statement (Val (Quoted "abc"))),
    "sv8" ~: succeed 
    (parseStatementTest "\"ab c\"") 
    (Statement (Val (Quoted "ab c"))),
    "sv9" ~: succeed
    (parseStatementTest "\"\"")
    (Statement (Val (Quoted ""))) ]

tStatementAssign :: Test
tStatementAssign =
  "Test Statement Assign" ~:
  TestList [
    "sa1" ~: succeed
    (parseStatementTest "a=2")
    (Statement (Assign "a" (Number 2))),
    "sa2" ~: succeed
    (parseStatementTest "b=haskell")
    (Statement (Assign "b" (String "haskell"))),
    "sa3" ~: succeed
    (parseStatementTest "b=\"\"")
    (Statement (Assign "b" (Quoted ""))),
    "sa4" ~: succeed
    (parseStatementTest "b=\"haskell\"")
    (Statement (Assign "b" (Quoted "haskell"))) ]

tStatementCommand :: Test
tStatementCommand =
  "Test Statement Command" ~:
  TestList [
    "sc1" ~: succeed
    (parseStatementTest "ls -lrt")
    (Statement (Command "ls" [String "-lrt"])),
    "sc2" ~: succeed
    (parseStatementTest "ls -l -r -t")
    (Statement (Command "ls" [String "-l", String "-r", String "-t"])),
    "sc3" ~: succeed
    (parseStatementTest "echo \"a b\"")
    (Statement (Command "echo" [Quoted "a b"])),
    "sc4" ~: succeed
    (parseStatementTest "echo 3")
    (Statement (Command "echo" [Number 3])),
    "sc5" ~: succeed
    (parseStatementTest "echo \"ab \"ab")
    (Statement (Command "echo" [Quoted "ab ",String "ab"]))]
    --"sc6" ~: succeed
    --(parseStatementTest "echo abcd\"")
    --(Statement (Command "echo" [String "abcd\""])) ]
    --"sc7" ~: failure
    --(parseStatementTest "echo \"ab") ]
    -- these two are future features