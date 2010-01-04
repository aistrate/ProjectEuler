import Prob003 (primeFactors)
import Prob038 (digitList)
import Data.List


isPandigital :: Integer -> Bool
isPandigital n = null (digits \\ [1..distinctCount])
                 where digits = digitList n
                       distinctCount = length (nub digits)

pandigitalsDesc n = [ p | p <- [n,n-1..1], isPandigital p ]


isPrime p = length (primeFactors p) == 1


-- 987654321 and 87654321 (and their permutations) are divisible by 3
result = head $ filter isPrime $ pandigitalsDesc 7654321
-- (538 total)
-- 7652413
