module Main where

import Prob003 (primeFactors, primeNumbers)
 
main = print (head solve)
 
solve = do
 a <- primesTo10000
 let m = f a $ dropWhile (<= a) primesTo10000
 b <- m
 let n = f b $ dropWhile (<= b) m
 c <- n
 let o = f c $ dropWhile (<= c) n
 d <- o
 -- return [a,b,c,d]
 let p = f d $ dropWhile (<= d) o
 e <- p
 return [a,b,c,d,e]
 where
  f x = filter (\y -> isPrime (read (shows x (show y)))
                && isPrime (read (shows y (show x))))


primesTo10000 = takeWhile (<= 10000) primeNumbers
isPrime = null . tail . primeFactors

{-
primesTo100 :: [Integer]
primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
 
trialDivision ps n = doTrialDivision ps
	where doTrialDivision (p:ps) = let (q,r) = n `quotRem` p in if r == 0 then False else if q < p then True else doTrialDivision ps
	      doTrialDivision [] = True
 
primesTo10000 = primesTo100 ++ filter (trialDivision primesTo100) [101,103..9999]
 
isTrialDivisionPrime 2 = True -- special case, not caught by above code
isTrialDivisionPrime n = trialDivision (primesTo10000 ++ [10001,10003..]) n
 
isPrime = isTrialDivisionPrime
-}
