import Data.Ratio
import Prob003 (primeFactors)

isPrime = null . tail . primeFactors

layer k = [4*k*k - 2*k + 1, 4*k*k + 1, 4*k*k + 2*k + 1]

spiralCounts = (0, 0, 1) : map nextLayer spiralCounts
    where nextLayer (n, primes, nrs) =
            let nextPrimes = length (filter isPrime $ layer (n+1))
            in (n + 1, primes + nextPrimes, nrs + 4)


primesRatio (n, primes, nrs) = primes % nrs

firstRatioUnder r = head $ filter ((< r) . primesRatio) (tail spiralCounts)
-- (13120,5248,52481)


main = print $
       let (n, _, _) = firstRatioUnder (1 % 10)
       in 2*n + 1
-- 26241
