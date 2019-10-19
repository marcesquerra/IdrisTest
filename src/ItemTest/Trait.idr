||| Collection of checks that try to ensure an item has one specific 'trait'
||| (or 'characteristic')

module ItemTest.Trait

import ItemTest.TestResult

public export
TraitCheck: Type -> Type
TraitCheck item = item -> TestResult


export
beTrue: TraitCheck Bool
beTrue True = Success
beTrue False = Failure "Expecting True, but got False"

export
beFalse: TraitCheck Bool
beFalse False = Success
beFalse True = Failure "Expecting False, but got True"

export
beEqualTo : Eq item => Show item => (other: item) -> TraitCheck item
beEqualTo other item =
  if item == other then Success
  else Failure ("Expecting " ++ (show other) ++ ", but " ++ (show item) ++ " found")

export
beNotEqualTo : Eq item => Show item => (other: item) -> TraitCheck item
beNotEqualTo other item =
  if item /= other then Success
  else Failure ("Both values are " ++ (show item))

public export
beEven : Eq item => Show item => Integral item => TraitCheck item
beEven item =
  if (mod item 2 == 0) then Success
  else Failure ("'" ++ (show item) ++ "' is not a pair number")

public export
beOdd : Eq item => Show item => Integral item => TraitCheck item
beOdd item =
  if (mod item 2 /= 0) then Success
  else Failure ("'" ++ (show item) ++ "' is not an odd number")


