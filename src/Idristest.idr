module Idristest

import System

public export
TestResult: Type
TestResult = (Bool, String)

public export
TestSuitResult: Type
TestSuitResult = List TestResult

exists : (a -> Bool) -> List a -> Bool
exists p l = isJust (find p l)

forall : (a -> Bool) -> List a -> Bool
forall p l = not (exists (\i => not (p i)) l)

printResults : TestSuitResult -> IO (List Bool)
printResults   tests = sequence (map (\pair => do putStrLn (snd pair)
                                                  pure (fst pair)) tests)

runTests : TestSuitResult -> IO Bool
runTests   tests = map (\l => forall id l) (printResults tests)

export
assert : Bool -> String -> String -> TestResult
assert   True    s         f      =  (True, s)
assert   False   s         f      =  (False, f)

export
assertEq : (Eq a, Show a) => (label: String) -> (given : a) -> (expected : a) -> TestResult
assertEq l g e = assert (g == e) (l ++ ": Passed") (l ++ ": Failed\n  GIVEN: " ++ (show g) ++ "\n  EXPECTED: " ++ (show e))

export
assertNotEq : (Eq a, Show a) => (label: String) -> (given : a) -> (expected : a) -> TestResult
assertNotEq l g e = assert ( not (g == e)) (l ++ ": Passed") (l ++ ": Failed\n  Both values were expected to be different\n  BOTH ARE: " ++ (show g) ++ "\n  EXPECTED: ")

export
run : TestSuitResult -> IO ()
run ts = do r <- runTests ts
            if r then
              putStrLn "Success"
            else (
              do putStrLn "Failure"
                 exit 1
            )
