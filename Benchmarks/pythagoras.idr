-- Shamelessly stolen from Edwin Brady
module Main

pythagoras : Int -> List (Int, Int, Int)
pythagoras max = [
    (x, y, z)
    | z <- [1..max]
    , y <- [1..z]
    , x <- [1..y]
    , x * x + y *y == z * z
  ]

main : IO ()
main = do
  [_, n] <- getArgs
  printLn $ pythagoras (cast n)
