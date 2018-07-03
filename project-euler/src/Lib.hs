module Lib where

-- First Fibonacci Numbers: 1, 1, 2, 3, 5, 8, 13
-- Sum of these that are even is 10.

fib :: [Int]
fib =
    map (uncurry (+)) (zip (1:fib) (0:1:fib))

problemTwo :: Int -> Int
problemTwo limit = sum $ filter even $ takeWhile (< limit) fib

-- ****************************************************************************

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome d =
    d == reverse d

digits :: Int -> [Int]
digits 0 = []
digits n = (digits q) ++ [r]
    where (q, r) = divMod n 10

problemThree :: Int
problemThree =
    maximum $ filter (isPalindrome . digits) [x * y | x <- r, y <- r]
    where r = [100..999]

-- ****************************************************************************

someFunc :: IO ()
someFunc = putStrLn "someFunc"
