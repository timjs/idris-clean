module pythagorasBoxedStrict

import StdEnv

:: List a :== [a]
:: StrictBox a = Box !a

instance + (StrictBox a) | + a where
    (+) (Box a) (Box b) = Box (a + b)
instance * (StrictBox a) | * a where
    (*) (Box a) (Box b) = Box (a * b)
instance < (StrictBox a) | < a where
    (<) (Box a) (Box b) = a < b
instance == (StrictBox a) | == a where
    (==) (Box a) (Box b) = a == b
instance one (StrictBox a) | one a where
    one = Box (one)

pythagoras :: (StrictBox Int) -> List (StrictBox Int, StrictBox Int, StrictBox Int)
pythagoras max = [
    (x, y, z)
    \\ z <- [Box 1..max]
    , y <- [Box 1..z]
    , x <- [Box 1..y]
    | x * x + y * y == z * z
  ]

Start = pythagoras (Box 300)
