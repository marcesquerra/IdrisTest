Simple testing library for Idris

A simple example:

```idris

import Idristest

export
runTests : IO ()
runTests = run (
  "This is a collection of tests" >> [
    "Checking the length" >> (length "hello") must be 5,

    "Some subtests are required" >> [
      "Testing some boolean" >> True must beTrue,
      "... and some more" >> (not True) must beFalse
    ]
  ]
)
```

