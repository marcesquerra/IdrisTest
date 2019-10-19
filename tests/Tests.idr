module Tests

import Idristest

foo: IO TestResult
foo = (pure True) evaluated must beTrue

foo2: IO TestResult
foo2 = True must beTrue

foo3: IO TestResult
foo3 = True must beFalse

foo4: IO TestResult
foo4 = True must beEqualTo False

foo5: IO TestResult
foo5 = (pure True) evaluated must beEqualTo False

foo6: IO TestResult
foo6 = (pure True) when evaluated must beNotEqualTo False

foo7: IO TestResult
foo7 = 5 must beEven

foo8: IO TestResult
foo8 = (pure 5) evaluated must beEven

foo9: IO TestResult
foo9 = (pure 5) when evaluated must beEven

bar: Suite
bar = "bar" >> True must beTrue

baz : Suite
baz =
  "This is a collection of tests" >> [
    "Checking the length" >> (length "hello") must beEqualTo 5,

    "Some subtests are required" >> [
      "Testing some boolean" >> True must beTrue,
      "... and some more" >> (not True) must beFalse
    ]
  ]


export
runTests : IO ()
runTests = run (
  "This is a collection of tests" >> [
    "Checking the length" >> (length "hello") must beEqualTo 5,

    "Some subtests are required" >> [
      "Testing some boolean" >> True must beTrue,
      "... and some more" >> (not True) must beFalse,
      "also wrapping an IO" >> (pure True) evaluated must beTrue
    ],

    "What about some IO entries?" >> 5 must beEqualTo (3 + 2),
    "And some other properties" >> 5 must beOdd ,
    "but then again..." >> 6 must beEven
  ]
)
