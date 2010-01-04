import Data.List
import Control.Monad


-- a = 2mn  :  b = m^2 - n^2  :  c = m^2 + n^2    (where m > n)
-- a + b + c = 2m(m + n)
-- m^2 + mn <= limit / 2
primitiveTriplets :: Integer -> [(Integer, Integer, Integer)]
primitiveTriplets limit = do m <- [2..intSqrt (limit `div` 2)]
                             n <- [1..m - 1]
                             guard $ gcd m n == 1 && odd (m + n)
                             let a = 2 * m * n
                             let b = m^2 - n^2
                             let c = m^2 + n^2
                             guard $ a + b + c <= limit
                             return (min a b, max a b, c)
    where intSqrt = truncate . sqrt . fromIntegral


allTriplets limit = concatMap multiples $ primitiveTriplets limit
    where multiples (a, b, c) = map (\k -> (k*a, k*b, k*c)) 
                                    [1..limit `div` (a + b + c)]


uniqueSums = map head
           . Data.List.filter ((== 1) . length)
           . group . sort
           . map (\(a, b, c) -> a + b + c) 


result = length . uniqueSums $ allTriplets 2000000
-- 214954


-- primitiveTriplets:  140465
-- allTriplets:       1715257
