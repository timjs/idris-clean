module pythagorasUnboxedLazy

import StdEnv

:: List a :== [a]

// Probably inferred strict by the compiler
pythagoras :: Int -> List (Int, Int, Int)
pythagoras max = [
    (x, y, z)
    \\ z <- [1..max]
    , y <- [1..z]
    , x <- [1..y]
    | x * x + y * y == z * z
  ]

Start = pythagoras 300
