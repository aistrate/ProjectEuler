module Prob003 where

primeFactors n = primeFactors' n primeNumbers

primeFactors' 1 _     = []
primeFactors' n (p:_)
    | p * p > n       = [n]
primeFactors' n primes@(p:ps)
    | n `mod` p == 0  = p : primeFactors' (n `div` p) primes
    | otherwise       = primeFactors' n ps


primeNumbers = primeNumbers' 2 []

primeNumbers' n primes =
    if any (n `dividedBy`) (takeWhile (`lessThanSqrtOf` n) primes)
        then primeNumbers' (n + 1) primes
        else n : primeNumbers' (n + 1) (primes ++ [n])
    where a `dividedBy` b       = a `mod` b == 0
          a `lessThanSqrtOf` b  = a * a <= b


result = maximum (primeFactors 600851475143)
-- [71,839,1471,6857]

-- main = print $ primeFactors 600851475147
