module pythagorasUnboxedStrict

import StdEnv

:: List a :== [a]

pythagoras :: !Int -> List !(!Int, !Int, !Int)
pythagoras max = [
    (x, y, z)
    \\ z <- [1..max]
    , y <- [1..z]
    , x <- [1..y]
    | x * x + y * y == z * z
  ]

Start = pythagoras 300
