module Prob069 (phi, phiFraction) where

import Data.List (elemIndex, nub, foldl1')
import Data.Maybe (fromJust)
import Data.Ratio
import Prob003 (primeFactors)
import Prob012 (combinations)


{-relativePrimes a b = gcd a b == 1

phi :: Int -> Int
phi n = length $ filter (relativePrimes n) [1..n-1]-}


combinationsOf ns xs = concat $ map ($ xs) (map combinations ns)

distPrimeFactors :: Int -> [Int]
distPrimeFactors = nub . map fromIntegral . primeFactors . fromIntegral

combOfPrimeFactors n = (divsOfOrders [1,3..len], 
                        divsOfOrders [2,4..len])
    where fs = distPrimeFactors n
          len = length fs
          divsOfOrders ks = map product $ combinationsOf ks fs

phi :: Int -> Int
phi n = n - (nonRelativePrimes oddFactors) + 
            (nonRelativePrimes evenFactors)
    where (oddFactors, evenFactors) = combOfPrimeFactors n
          nonRelativePrimes xs = sum $ map (n `div`) xs


phiFraction :: Int -> Double
phiFraction n = (fromIntegral n) / (fromIntegral (phi n))

maxPhiFraction n = fst $ foldl1' maxPair phiPairs
    where phiPairs = map (\k -> (k, phiFraction k)) [2..n]
          maxPair p q | snd p >= snd q = p
                      | otherwise      = q


result = maxPhiFraction 1000000

main = print $ result
-- Compiled: 510510
-- 5.539388020833333
