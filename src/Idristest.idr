module Idristest

import System

export
data TestResult = Success | Failure String

export
eval : a -> (a -> TestResult) -> TestResult
eval a f = f a

term syntax [initial] must [test] = eval initial test

export
beTrue : Bool -> TestResult
beTrue True = Success
beTrue False = Failure "Expecting True, but got False"

export
beFalse : Bool -> TestResult
beFalse False = Success
beFalse True = Failure "Expecting False, but got True"

export
be : Eq a => Show a => a -> a -> TestResult
be x y =
  if x == y then Success
  else Failure ("Expecting " ++ (show x) ++ ", but " ++ (show y) ++ " found")

export
notBe : Eq a => Show a => a -> a -> TestResult
notBe x y =
  if x /= y then Success
  else Failure ("Both values are " ++ (show x))

export
data Suite = Group String (List Suite) | Test String TestResult

public export
interface SuiteBuilder a where
  buildSuite : String -> a -> Suite

public export
SuiteBuilder TestResult where
  buildSuite description tr = Test description tr

public export
SuiteBuilder (List Suite) where
  buildSuite description ls = Group description ls

term syntax [description] ">>" [content] = buildSuite description content

record SuiteResult where
  constructor MkSuiteResult
  successful, failed: Nat
  out: String

(*) : String -> Nat -> String
(*) str Z = ""
(*) str (S n) = str ++ (str * n)

mutual
  evalSuiteList: (depth: Nat) -> List Suite -> SuiteResult
  evalSuiteList _ Nil = MkSuiteResult 0 0 ""
  evalSuiteList d (h :: t) =
    let
      headResult = evalSuiteEntry d h
      tailResult = evalSuiteList d t
      successfuls = (successful headResult) + (successful tailResult)
      failures = (failed headResult) + (failed tailResult)
      fullOut = (out headResult) ++ (out tailResult)
    in
      MkSuiteResult successfuls failures fullOut

  evalSuiteEntry: (depth: Nat) -> Suite -> SuiteResult
  evalSuiteEntry depth (Test desc Success) = MkSuiteResult 1 0 ("  " * depth ++ desc ++ " [✓]\n")
  evalSuiteEntry depth (Test desc (Failure errorDesc)) = MkSuiteResult 0 1 ("  " * depth ++ desc ++ " [✗]\n" ++ "  " * depth ++ errorDesc ++ "\n\n")
  evalSuiteEntry depth (Group desc list) =
    let
      innerResult = evalSuiteList (depth + 1) list
      newOut = "  " * depth ++ desc ++ "\n" ++ (out innerResult)
    in
      MkSuiteResult (successful innerResult) (failed innerResult) newOut

evalSuite : Suite -> SuiteResult
evalSuite s = evalSuiteEntry 0 s

export
run : Suite -> IO ()
run s =
  let
    result = evalSuite s
    totalTests = (successful result) + (failed result)
    ok = failed result == 0
    sufix = if ok then "\nSuccessful: " ++ (show (successful result)) ++ "/" ++ (show totalTests) ++ "\n\nSUCCES"
            else "\nFailed: " ++ (show (failed result)) ++ "/" ++ (show totalTests) ++ "\n\nFAILED"
    fullOut = (out result) ++ sufix
  in
    do
      putStrLn fullOut
      (if ok then pure () else exit 1)
