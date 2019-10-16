module SingleTest

public export
interface Lifter candidate inner where
  lift : candidate -> IO inner

public export
Lifter (IO a) a where
  lift ioa = ioa

public export
Lifter a a where
  lift a = pure a

public export
data TestResult = Success | Failure String

export
eval : Lifter c i => c -> (i -> TestResult) -> IO TestResult
eval c f = map f (lift c)

term syntax [initial] must [test] = eval initial test

export
beTrue: Bool -> TestResult
beTrue True = Success
beTrue False = Failure "Expecting True, but got False"

export
beFalse: Bool -> TestResult
beFalse False = Success
beFalse True = Failure "Expecting False, but got True"

export
be : Eq a => Show a =>  a -> a -> TestResult
be x y =
  if x == y then Success
  else Failure ("Expecting " ++ (show x) ++ ", but " ++ (show y) ++ " found")

export
notBe : Eq a => Show a => a -> a -> TestResult
notBe x y =
  if x /= y then Success
  else Failure ("Both values are " ++ (show x))
