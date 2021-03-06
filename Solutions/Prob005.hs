import Data.List ((\\), product)
import Prob003 (primeFactors)

allDivisorsTo 2 = [2]
allDivisorsTo n = let ds = allDivisorsTo (n - 1)
                  in ds ++ (primeFactors n \\ ds)

smallestDivisible n = product (allDivisorsTo n)

main = print $
       smallestDivisible 20
-- 232792560
