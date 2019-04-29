module ThreeNPlusOne
  ( solution
  ) where

solution :: IO ()
solution = putStrLn "wip"

-- output of three ints—input plus maximum output of any number between inputs
-- inclusive

threeNPlus1 :: Int -> Int -> Int
threeNPlus1 int1 int2 = maximum $ map (recursor 1) [int1..int2]

recursor :: Int -> Int -> Int
recursor count value
  | 1 == value = succ count
  | odd value = recursor (succ count) (3 * value + 1)
  | even value = recursor (succ count) (value `div` 2)
