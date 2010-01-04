module Prob068 (arrangements) where

import Data.List (permutations, sort, sortBy, groupBy)
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


canFollow inside outside s t = (not $ any (`elem` outside) t) &&
                               (not $ (head t) `elem` inside) &&
                               (head . tail $ tail s) == (head $ tail t)

findChains :: Int -> [Int] -> [Int] -> [[Int]] -> [Int] -> [[[Int]]]
findChains 1 _ _ _ x             = [[x]]
findChains n inside outside xs x = map (x:) chains
    where newInside = (tail x) ++ inside
          newOutside = (head x):outside
          nexts = filter (canFollow newInside newOutside x) xs
          chains = concat $ map (findChains (n-1) newInside newOutside xs) nexts

findAllChains n xs = concat $ map (findChains n [] [] xs) xs


isCircular xs = canFollow [] [] (last xs) (head xs)

isLowestExternalFirst xs = (head $ head xs) == minimum externals
    where externals = map head xs

solutionSet xs = filter isCircular . filter isLowestExternalFirst $ solutions
    where len = tripletsInFigure xs
          solutions = concat . map (findAllChains len) $ tripletGroups xs


toDigits :: [[Int]] -> Integer
toDigits = makeNr . map fromIntegral . concat . map expandDoubleDigits . concat
    where expandDoubleDigits n | n < 10    = [n]
                               | otherwise = [n `div` 10, n `mod` 10]


result = maximum . filter ((== 16) . length . show) . map toDigits $ solutionSet [1..10]
-- 6531031914842725
