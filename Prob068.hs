module Prob068 (arrangements) where

import Data.List (permutations, sort, sortBy, groupBy, group)
import Prob012 (combinations)
import Prob035 (makeNr)
import Prob038 (digitList)


arrangements k xs = sort . concat . map permutations $ combinations k xs


groupBySum :: [[Int]] -> [[[Int]]]
groupBySum = map (map fst) . groupBy sameSum . sortBy compPairs . map mkPair
    where mkPair xs = (xs, sum xs)
          compPairs p q = compare (snd p) (snd q)
          sameSum p q = (snd p) == (snd q)

tripletsInFigure = (`div` 2) . length

tripletGroups xs = filter ((> minGroupLen). length) . groupBySum $ arrangements 3 xs
    where minGroupLen = 2 * (tripletsInFigure xs)


canFollow inside outside s t = (null outside || head t > last outside) &&
                               (not $ any (`elem` outside) t) &&
                               (not $ any (`elem` inside) (init t)) &&
                               (head . tail $ tail s) == (head $ tail t)

findChains :: Int -> [Int] -> [Int] -> [[Int]] -> [Int] -> [[[Int]]]
findChains 1 _ _ _ x             = [[x]]
findChains n inside outside xs x = map (x:) chains
    where newInside = (head $ tail x):inside
          newOutside = (head x):outside
          nexts = filter (canFollow newInside newOutside x) xs
          chains = concat $ map (findChains (n-1) newInside newOutside xs) nexts

findAllChains n xs = concat $ map (findChains n [] [] xs) xs


isCircular xs = canFollow [] [] (last xs) (head xs)

isUsingEntireSet xs ts = xs == (map head . group . sort . concat $ ts)

solutionSet xs = filter (isUsingEntireSet xs) . filter isCircular $ solutions
    where len = tripletsInFigure xs
          solutions = concat . map (findAllChains len) $ tripletGroups xs


toDigits :: [[Int]] -> Integer
toDigits = makeNr . map fromIntegral . concat . map expandDoubleDigits . concat
    where expandDoubleDigits n | n < 10    = [n]
                               | otherwise = [n `div` 10, n `mod` 10]


main = print $
       maximum . filter ((== 16) . length . show) . map toDigits $ solutionSet [1..10]
-- 6531031914842725


-- for [1..6], [1..8], ..., [1..16], there are 8, 12, 12, 40, 236, 564 solutions, respectively;
-- first solution for [1..18] :
--      [[10,5,9],[11,9,4],[12,4,8],[13,8,3],[14,3,7],[15,7,2],[16,2,6],[17,6,1],[18,1,5]]
