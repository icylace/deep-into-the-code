module FFI where

import Data.Function.Uncurried (Fn2, runFn2)

foreign import calculateInterest :: Number -> Number
foreign import calculateInterest2 :: Fn2 Number Number Number
foreign import calculateInterest3 :: Number -> Number -> Number

calculateInterest2Curried :: Number -> Number -> Number
calculateInterest2Curried = runFn2 calculateInterest2
