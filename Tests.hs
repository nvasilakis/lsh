module Tests where

import Test.HUnit
import Types
import Parser
import Text.ParserCombinators.Parsec

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
    --"sv3" ~: succeed (parseStatementTest "-1") (Statement (Val (Number (-1)))),
    "sv4" ~: succeed 
    (parseStatementTest "   1") 
    (Statement (Val (Number (1)))),
    "sv5" ~: succeed 
    (parseStatementTest "   1    ") 
    (Statement (Val (Number (1)))),
    "sv6" ~: succeed 
    (parseStatementTest "1    ") 
    (Statement (Val (Number (1)))),
    "sv7" ~: succeed 
    (parseStatementTest "abc") 
    (Statement (Val (String "abc"))),
    --"sv8" ~: succeed (parseStatementTest "") (Statement (Val (String ""))),
    "sv9" ~: succeed 
    (parseStatementTest "\"abc\"") 
    (Statement (Val (Quoted "abc"))),
    --"sv10" ~: succeed (parseStatementTest "\"ab c\"") (Statement (Val (Quoted "ab c"))),
    "sv11" ~: succeed 
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
    (Statement (Command "echo" [Quoted "ab ",String "ab"])), 
    "sc6" ~: succeed 
    (parseStatementTest "echo abcd\"") 
    (Statement (Command "echo" [String "abcd\""])) ]
    --"sc7" ~: failure
    --(parseStatementTest "echo \"ab") ]