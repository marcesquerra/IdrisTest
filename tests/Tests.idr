module Tests

import Idristest

foo: IO TestResult
foo = (pure True) must beTrue

foo2: IO TestResult
foo2 = True must beTrue

foo3: IO TestResult
foo3 = True must beFalse

foo4: IO TestResult
foo4 = True must be False

foo5: IO TestResult
foo5 = (pure True) must be False

foo6: IO TestResult
foo6 = (pure True) must notBe False

bar: Suite
bar = "bar" >> True must beTrue

baz : Suite
baz =
  "This is a collection of tests" >> [
    "Checking the length" >> (length "hello") must be (the Nat 5),

    "Some subtests are required" >> [
      "Testing some boolean" >> True must beTrue,
      "... and some more" >> (not True) must beFalse
    ]
  ]


export
runTests : IO ()
runTests = run (
  "This is a collection of tests" >> [
    "Checking the length" >> (length "hello") must be (the Nat 5),

    "Some subtests are required" >> [
      "Testing some boolean" >> True must beTrue,
      "... and some more" >> (not True) must beFalse
    ],

    "What about some IO entries?" >> (pure 5) must be (3 + 2)
  ]
)
