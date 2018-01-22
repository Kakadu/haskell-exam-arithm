module Main where

import Arithm (eval, parse, parsingOps)
import Test.HUnit
import Data.Either
import System.Exit

test1 name str expected =
  TestLabel name $ TestList [c1,c2]
  where
    rez = Arithm.parse parsingOps str
    c1 = TestCase (assertBool "parsable" (isRight rez))
    (Right tree) = rez
    c2 = TestCase (assertEqual "parsable" (Arithm.eval tree) expected)

tests = TestList
  [ test1 "test1" "1+2*3" 7
  , test1 "test2" "5*3" 15
  ]

main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
   then
     exitWith ExitSuccess
   else
     exitWith (ExitFailure 1)
