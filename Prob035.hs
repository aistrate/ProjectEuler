module Prob035 (makeNr) where

import Data.List (foldl')
import Data.Char (digitToInt)
import Data.Array
import Prob003 (primeNumbers)
import Prob027 (isPrimeList)

makeNr = foldl' addDigit 0
         where addDigit s d = 10 * s + d

rotations :: Integer -> [Integer]
rotations n = map makeNr $ rotations' (length digits - 1) digits
    where rotations' 0 _      = []
          rotations' 1 ds     = [rotate ds]
          rotations' times ds = (rotate ds) : 
                                (rotations' (times - 1) (rotate ds))
          digits = digitList n
          rotate (m:ms) = ms ++ [m]

digitList n = map (fromIntegral . digitToInt) $ show n


maxPrime = 1000000
primes = listArray (1, maxPrime) isPrimeList


isCircular p = all (primes !) (rotations p)

circulars n = filter isCircular $ takeWhile (<= n) primeNumbers


result = length $ circulars 1000000
{-
[2,3,5,7,11,13,17,31,37,71,73,79,97,113,131,197,199,311,337,373,719,733,
 919,971,991,1193,1931,3119,3779,7793,7937,9311,9377,11939,19391,19937,
 37199,39119,71993,91193,93719,93911,99371,193939,199933,319993,331999,
 391939,393919,919393,933199,939193,939391,993319,999331]
-}
-- 55
