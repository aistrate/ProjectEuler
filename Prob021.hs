module Prob021 where

import Prob012 (allFactors)

properDivisors = init . allFactors

amicablePair a b = sum (properDivisors a) == b && sum (properDivisors b) == a

amicable a = let b = sum (properDivisors a)
             in b /= a && sum (properDivisors b) == a


result = sum $ filter amicable [2..10000]
-- [220,284,1184,1210,2620,2924,5020,5564,6232,6368]
-- 31626
