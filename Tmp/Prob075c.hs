import Data.List
import Data.Map as M hiding (map)
import Control.Monad


limit :: Integer
limit = 2000000

squares        = M.fromList $ map (\i -> (i, i ^ 2)) [1..limit `div` 2]
squaresInverse = M.fromList $ map (\i -> (i ^ 2, i)) [1..limit `div` 2]


triplets :: [(Integer, Integer, Integer)]
triplets = do a <- [1..(limit `div` 3)]
              b <- [a + 1..((limit - a) `div` 2)]
              let c2 = squares M.! a + squares M.! b
              guard $ member c2 squaresInverse
              let c = squaresInverse M.! c2
              guard $ a + b + c <= limit
              return (a, b, c)


-- a = 2mn  :  b = m^2 - n^2  :  c = m^2 + n^2    (where m > n)
-- a + b + c = 2m(m + n)
-- m^2 + mn <= limit / 2
primitiveTriplets :: [(Integer, Integer, Integer)]
primitiveTriplets = do m <- [2..intSqrt (limit `div` 2)]
                       n <- [1..m - 1]
                       guard $ gcd m n == 1 && odd (m + n)
                       let a = 2 * m * n
                       let b = m^2 - n^2
                       let c = m^2 + n^2
                       guard $ a + b + c <= limit
                       return (min a b, max a b, c)

intSqrt = truncate . sqrt . fromIntegral

allTriplets = concatMap multiples primitiveTriplets
    where multiples (a, b, c) = map (\k -> (k*a, k*b, k*c)) 
                                    [1..limit `div` (a + b + c)]


result = map head
       . Data.List.filter ((== 1) . length)
       . group . sort
       . map (\(a, b, c) -> a + b + c) 
       $ triplets

result2 = length primitiveTriplets


main = print $ length result
-- to 2000:  222
-- to 20000: 2209   (22 sec.)
-- to 50000: 5502   (142 sec.)


-- primitiveTriplets:  140465
-- allTriplets:       1715257
