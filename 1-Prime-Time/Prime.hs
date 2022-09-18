module Prime where

-- https://brilliant.org/wiki/prime-testing/
isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = factorialMod (n-1) n == n - 1


factorialMod :: Integer -> Integer -> Integer
factorialMod n m =
    step n m 1 1
        where step n m acc i
                | i > n = acc
                | otherwise = step n m (mod (acc * i) m) (i+1)
