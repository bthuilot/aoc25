{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}


import Data.Maybe
import Data.List
import System.Exit

import Days
import AOC

main :: IO ()
main = do
  testResults <- mapM runTestCase testCases
  if any isFailed testResults then exitFailure else exitSuccess
  where
    isFailed (TestFail _) = True
    isFailed _ = False

testCases :: [TestCase]
testCases =
  [ TestCase{ day = 0
            , part = 1
            , input = "hello, world!"
            , expected = "hello, world!"
            }
  , TestCase{ day = 0
            , part = 2
            , input = "hello, world!"
            , expected = "HELLO, WORLD!"
            }
  ]

data TestCase = TestCase
  { day :: Int
  , part :: Int
  , input :: String
  , expected :: String
  }

data TestResult = TestPass | TestFail String | TestSkippedNotImplemeted | TestSkippedNoInput


printTestResult :: Int -> Int -> TestResult -> IO ()
printTestResult dayNum partNum tr = putStrLn output
  where
    output = "day " ++ show dayNum ++ " part " ++ show partNum ++ ": " ++ showResult tr
    showResult TestPass = "pass"
    showResult TestSkippedNotImplemeted = "skipped - not implemeted"
    showResult TestSkippedNoInput = "skipped - no input"
    showResult (TestFail s) = "failed - " ++ s


runTestCase :: TestCase -> IO TestResult
runTestCase tc = do
  let testResult = evalResult (solution i)
  printTestResult dayNum partNum testResult
  return testResult
  where
    (i, e, dayNum, partNum) = (input tc, expected tc, day tc, part tc - 1)
    solution = fromMaybe notImplemented (getDay dayNum !? partNum)
    evalResult (Ok output) = if e == output then TestPass else TestFail (e ++ " did not match " ++ output)
    evalResult NotImplemented = TestSkippedNotImplemeted
    evalResult _ = TestFail "unable to run"

  