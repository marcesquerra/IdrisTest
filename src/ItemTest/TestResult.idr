module ItemTest.TestResult

||| Describes the result of running a single test
public export
data TestResult =

  ||| The test has passed
  Success |

  ||| The test has failed, including a description of what has gone wrong
  Failure String
