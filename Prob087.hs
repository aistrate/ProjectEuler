import Prob003 (primeNumbers)
import Data.List


powerSums limit = map head . group $ sort sumsThruA
    where sumsThruA     = concat $ takeWhile (not . null) [ sumsThruB a | a <- primeNumbers ]
          sumsThruB a   = concat $ takeWhile (not . null) [ sumsThruC a b | b <- primeNumbers ]
          sumsThruC a b = takeWhile (<= limit) [ a^2 + b^3 + c^4 | c <- primeNumbers ]


result = length $ powerSums (50000000 - 1)
-- 1097343      (1139575 before eliminating duplicates)
