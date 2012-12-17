import Types
import Parser
import Evaluator
import ConfigFile
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim hiding (try)
import qualified Text.ParserCombinators.Parsec.Token as PT
import Test.QuickCheck
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import System.Environment
import System.Directory
import Data.Char

--quickcheck start
  
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
                         (3, aalias),
                         (2, apipe),
                         (1, ahigher)
                        ] 
    where astate = liftM Statement arbitrary
          apipe = oneof [liftM2 Pipe arbitrary astate,
                         liftM2 Pipe arbitrary ahigher]
          aalias = liftM Alias (oneof 
                                [elements [
                                    [String "a='b'"],
                                    [String "cd='sdf'",String "ls='sdf'"]]])
          ahigher = oneof [liftM3 Higher arbitrary arbitrary arbitrary]
  
  shrink _ = []
  
instance Arbitrary Complete where
  arbitrary = oneof [acomplex,
                     liftM2 Ssep arbitrary acomplex]
    where acomplex = liftM Complex arbitrary
  shrink _ = []

--quickCheck properties

checkCompleteParser st = 
  case (parse parseComplete "Shell Parser" (display st)) of
    Left  _ -> False
    Right a -> st == a
  where types = st :: Complete

main :: IO ()
main = do
  runStaticTests
  --verboseCheck checkComplexParser
  quickCheck checkCompleteParser
  
--quickcheck end  
  
runStaticTests :: IO Counts
runStaticTests = runTestTT $ TestList testList

testList :: [Test]
testList = [ tStatementVal,
             tStatementAssign,
             tStatementCommand,
              tHigherTests ]

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

evalHigherFunctionTest :: String -> IO String
evalHigherFunctionTest str = do
  let complex = case (parse parseComplex "Shell Parser" str) of 
                  Left  _ -> Noop
                  Right a -> a
  uniResult <- sh complex Redirect defaultUni
  return $ unlines(Types.output uniResult)

succeedHigher :: String -> String -> Assertion
succeedHigher input expected = do
                    actual <- evalHigherFunctionTest input 
                    if actual == expected
                       then assert True
                       else assert False                    

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

tHigherTests :: Test
tHigherTests = 
   "Test Higher Functions" ~:
    TestList [
     "sc1" ~: succeedHigher "filter (grep cabal) (ls)" "lsh.cabal\n",
     "sc2" ~: succeedHigher "filter (grep cab) (ls)" "lsh.cabal\n"]