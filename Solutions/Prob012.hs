module Prob012 (main, allFactors, combinations, allCombinations, makeUnique) where

import Data.List (sort, group, (\\), find)
import Data.Maybe (fromJust)
import Prob003 (primeFactors)

triangles = triangles' 1 0
            where triangles' n sn = (n + sn) : triangles' (n + 1) (n + sn)


combinations :: Ord a => Int -> [a] -> [[a]]
combinations 1 xs = map (:[]) xs
combinations n xs = makeUnique . concat $ map biggerComb (combinations (n - 1) xs)
                    where biggerComb comb = map (sort . (: comb)) (xs \\ comb)

allCombinations xs = concat [ combinations i xs | i <- [1..(length xs)] ]


allFactors n = makeUnique . map product . allCombinations $ 1 : primeFactors n


makeUnique :: (Eq a, Ord a) => [a] -> [a]
makeUnique = map head . group . sort


result = fromJust $ find ((> 500) . length . allFactors) triangles

main = do print result
          print (length $ allFactors result)
-- compiled:
-- 76576500
-- 576 factors
