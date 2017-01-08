module pythagorasBoxedLazy

import StdEnv

:: List a :== [a]
:: LazyBox a = Box a

instance + (LazyBox a) | + a where
    (+) (Box a) (Box b) = Box (a + b)
instance * (LazyBox a) | * a where
    (*) (Box a) (Box b) = Box (a * b)
instance < (LazyBox a) | < a where
    (<) (Box a) (Box b) = a < b
instance == (LazyBox a) | == a where
    (==) (Box a) (Box b) = a == b
instance one (LazyBox a) | one a where
    one = Box (one)

pythagoras :: (LazyBox Int) -> List (LazyBox Int, LazyBox Int, LazyBox Int)
pythagoras max = [
    (x, y, z)
    \\ z <- [Box 1..max]
    , y <- [Box 1..z]
    , x <- [Box 1..y]
    | x * x + y * y == z * z
  ]

Start = pythagoras (Box 300)
