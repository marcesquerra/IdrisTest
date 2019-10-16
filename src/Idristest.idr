module Idristest

import System
import public SingleTest
import Utils

export
data Suite = Group String (List Suite) | Test String (IO TestResult)

public export
interface SuiteBuilder a where
  buildSuite : String -> a -> Suite

public export
SuiteBuilder (IO TestResult) where
  buildSuite description iotr = Test description iotr

public export
SuiteBuilder (List Suite) where
  buildSuite description ls = Group description ls

term syntax [description] ">>" [content] = buildSuite description content

tab: String
tab = "  "

record SuiteResult where
  constructor MkSuiteResult
  successful, failed: Nat

evalSingleTest: Nat -> String -> TestResult -> IO SuiteResult
evalSingleTest depth desc Success = do
  putStrLn ((tab * depth) ++ desc ++ " [✓]")
  pure (MkSuiteResult 1 0)
evalSingleTest depth desc (Failure errorDesc) = do
  putStrLn ((tab * depth) ++ desc ++ " [✗]")
  putStrLn ((tab * depth) ++ errorDesc ++ "\n")
  pure (MkSuiteResult 0 1)

mutual
  evalSuiteList: (depth: Nat) -> List Suite -> IO SuiteResult
  evalSuiteList _ Nil = pure (MkSuiteResult 0 0)
  evalSuiteList d (h :: t) =
    do
      headResult <- evalSuiteEntry d h
      tailResult <- evalSuiteList d t
      successfuls <- pure ((successful headResult) + (successful tailResult))
      failures <- pure ((failed headResult) + (failed tailResult))
      pure (MkSuiteResult successfuls failures)

  evalSuiteEntry: (depth: Nat) -> Suite -> IO SuiteResult
  evalSuiteEntry depth (Test desc singeTestResult) =
    singeTestResult >>= (evalSingleTest depth desc)
  evalSuiteEntry depth (Group desc list) = do
      putStrLn (tab * depth ++ desc)
      innerResult <- evalSuiteList (depth + 1) list
      pure (MkSuiteResult (successful innerResult) (failed innerResult))

evalSuite : Suite -> IO SuiteResult
evalSuite s = evalSuiteEntry 0 s

export
run : Suite -> IO ()
run s =
  (evalSuite s) >>= evalResult where
    evalResult: SuiteResult -> IO ()
    evalResult result =
      let
        totalTests = (successful result) + (failed result)
        ok = failed result == 0
        report = if ok then "\nSuccessful: " ++ (show (successful result)) ++ "/" ++ (show totalTests) ++ "\n\nSUCCES"
                 else "\nFailed: " ++ (show (failed result)) ++ "/" ++ (show totalTests) ++ "\n\nFAILED"
      in
        do
          putStrLn report
          (if ok then pure () else exit 1)
