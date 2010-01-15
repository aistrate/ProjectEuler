-- 1/(n + a) + 1/b = 1/n
-- n(n + a + b) = b(n + a)
-- n(n + a) = ab
-- b = n^2/a + n
-- there is a solution 'a' for every divisor of n^2
-- (half of which are identical if a and b are swapped);
-- if n = d1^p1 * d2^p2 * ... dk^pk (where d1, d2, ..., dk are primes),
-- then n has (2*p1 + 1) * (2*p2 + 1) * ... * (2*pk + 1) divisors

module Prob108 (minSolutions) where

import Data.List (minimumBy)
import Timer


solutions :: [Int] -> Int
solutions ps = (product (map (\p -> 2*p + 1) ps) + 1) `div` 2

candidatePowers :: Int -> [[Int]]
candidatePowers n = let maxP = round $ logBase 2 (fromIntegral n)
                    in map reverse $ candidatePowers' [] 1 maxP
  where candidatePowers' ps mn mx | mn > mx   = []
                                  | otherwise = let ps' = mn : ps
                                                in if solutions ps' > n then [ps']
                                                   else candidatePowers' ps' 1 mn ++
                                                        candidatePowers' ps (mn + 1) mx


primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]

powers2num :: [Int] -> Integer
powers2num ps = product $ zipWith (^) primes ps


minSolutions :: Int -> (Integer, Int)
minSolutions n = let cs = map (\ps -> (powers2num ps, solutions ps)) 
                          $ candidatePowers n
                 in minimumBy (\a b -> compare (fst a) (fst b)) cs


main = printTime . print $ minSolutions 1000
-- (180180,1013)
