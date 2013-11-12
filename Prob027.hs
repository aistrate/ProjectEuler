module Prob027 (isPrimeList, primes) where

import Data.Array
import Data.List (sortBy)

import Prob003 (primeNumbers)

isPrimeList = isPrimeList' 1 (1 : primeNumbers)
    where isPrimeList' n primes@(p:ps) 
            | n < p     = False : isPrimeList' (n + 1) primes
            | otherwise = True  : isPrimeList' (n + 1) ps

maxPrime = 20000
primes = listArray (1, maxPrime) isPrimeList


primeSeq a b = primeSeq' 0
  where primeSeq' n | quadExp <= 0     = []
                    | primes ! quadExp = quadExp : primeSeq' (n + 1)
                    | otherwise        = []
            where quadExp = n * n + a * n + b


limit = 1000
primeSequences =
        [ (a, b, length sq, sq) | a <- [(-limit)..limit],
                                  b <- [(-limit)..limit],
                                  let sq = primeSeq a b,
                                  length sq > 1 ]

longestSeq = head . reverse $ sortBy comp primeSequences
    where comp (_, _, p, _) (_, _, q, _) = compare p q
{-
(-61, 971, 71,
    [971,911,853,797,743,691,641,593,547,503,461,421,383,347,313,281,251
    ,223,197,173,151,131,113,97,83,71,61,53,47,43,41,41,43,47,53,61,71,83
    ,97,113,131,151,173,197,223,251,281,313,347,383,421,461,503,547,593
    ,641,691,743,797,853,911,971,1033,1097,1163,1231,1301,1373,1447,1523
    ,1601])
-}

main = print $
       a * b
         where (a, b, _, _) = longestSeq
-- -59231
