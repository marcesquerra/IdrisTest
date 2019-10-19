Simple testing library for Idris

A simple example:

```idris

import Idristest

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
```

