module Prob012 (main, allFactors, combinations, allCombinations) where

import Data.List (nub)
import Prob003 (primeFactors)

triangles = triangles' 1 0
            where triangles' n sn = (n + sn) : triangles' (n + 1) (n + sn)


combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations n []     = []
combinations n (x:xs) = (map (x:) (combinations (n - 1) xs)) ++
                        (combinations n xs)

allCombinations xs = concat [ combinations i xs | i <- [1..length xs] ]


allFactors 1 = [1]
allFactors n = (1 :) . nub . map product . allCombinations $ primeFactors n


main = print $
       head $ filter ((> 500) . length . allFactors) triangles
-- 76576500
