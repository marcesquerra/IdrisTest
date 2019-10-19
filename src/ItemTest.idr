|||
module ItemTest

import public ItemTest.TestResult
import public ItemTest.Trait

term syntax [item] must [haveTrait] = pure (haveTrait item)
term syntax [item] evaluated must [haveTrait] = map haveTrait item
term syntax [item] when evaluated must [haveTrait] = map haveTrait item
