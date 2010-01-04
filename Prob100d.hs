-- (n / (n + m)) * ((n - 1) / (n + m - 1)) == 1/2

-- (n * (n - 1)) / ((n + m) * (n + m - 1)) == 1/2

-- 2 * n * (n - 1) == (n + m) * (n + m - 1)

-- 2n^2 - 2n == n^2 + nm - n + nm + m^2 - m

-- n^2 - n == m^2 + 2nm - m

-- m^2 + (2n - 1)m + (n - n^2) == 0

-- delta = b^2 - 4ac
-- m1 = (-b + sqrt(delta)) / 2a
-- m2 = (-b - sqrt(delta)) / 2a

-- a = 1
-- b = 2n - 1
-- c = n - n^2

-- delta = (2n - 1)^2 - 4(n - n^2)
--       = 4n^2 - 4n + 1 - 4n + 4n^2
--       = 8n^2 - 8n + 1

-- m = (sqrt(delta) + 1 - 2n) / 2
-- m = (sqrt(delta) + 1) / 2  - n

-- n + m == (sqrt(delta) + 1) / 2

-- [ minNPlusM = 10^12 + 1 ]

-- (sqrt(delta) + 1) / 2 >= minNPlusM
-- sqrt(delta) >= 2 * minNPlusM - 1
-- delta >= (2 * minNPlusM - 1)^2

-- 8n^2 - 8n + 1 >= (2 * minNPlusM - 1)^2
-- 8n^2 - 8n + 1 - (2 * minNPlusM - 1)^2 >= 0

-- a = 8; b = -8; c = 1 - (2 * minNPlusM - 1)^2
-- delta = 8^2 - 4 * 8 * (1 - (2 * minNPlusM - 1)^2)
--       = 32 ((2 * minNPlusM - 1)^2 + 1)

-- n >= (-8 + sqrt(delta)) / 16
-- n >= (4 * sqrt(2 * ((2 * minNPlusM - 1)^2 + 1)) + 8) / 16

-- n >= (sqrt(2 * ((2 * minNPlusM - 1)^2 + 1)) + 2) / 4

-- ex: minNPlusM = 20
--     ==> n >= (sqrt(2 * ((2 * 20 - 1)^2 + 1)) + 2) / 4
--           =  (sqrt(2 * (39^2 + 1)) + 2) / 4
--           =  (sqrt(3044) + 2) / 4
--           =~ (55.172 + 2) / 4
--           =  14.293
--         n >= 15


-- Algorithm:
-- Start with n = ceiling((sqrt(2 * ((2 * minNPlusM - 1)^2 + 1)) + 2) / 4)
-- Find minimum n for which delta = 8n^2 - 8n + 1 is a square number


import Data.Ratio
import Data.Maybe (fromJust)


calcMinN :: Integer -> Integer
calcMinN minNPlusM = ceiling ((sqrt (2 * ((2 * (fromIntegral minNPlusM) - 1)^2 + 1)) + 2) / 4)


equalElems :: [Integer] -> [Integer] -> [Integer]
equalElems (x:xs) (y:ys) | x == y    = x : equalElems xs ys
                         | x <  y    = equalElems (dropWhile (< y) xs) (y:ys)
                         | otherwise = equalElems (x:xs) (dropWhile (< x) ys)


squareDeltas :: Integer -> [Integer]
squareDeltas minN = let calcDelta n = 8*n^2 - 8*n + 1
                    in filter (\d -> d == (intSqrt d)^2) $ map calcDelta [minN..]


intSqrt :: Integral a => a -> a
intSqrt = truncate . sqrt . fromIntegral


nFromDelta :: Integer -> Integer
nFromDelta delta = let d = 64 + 32 * (delta - 1)
                   in (8 + intSqrt d) `div` 16


nmPairs :: Integer -> [(Integer, Integer)]
nmPairs minN = let nmPair delta = let n = nFromDelta delta 
                                      m = (intSqrt delta + 1) `div` 2 - n
                                  in (n, m)
               in map nmPair . squareDeltas $ minN


probability :: (Integer, Integer) -> Rational
probability (n, m) = let nn = n % 1
                         mm = m % 1
                     in nn / (nn + mm) * (nn - 1) / (nn + mm - 1)


main = let printPair p@(n, m) = do print p
                                   print $ n + m
                                   print $ probability p
                                   putStrLn ""
       in mapM_ printPair . take 1 . nmPairs . calcMinN $ 10^8 + 1


-- For n + m >= 10^6 + 1:
-- (3312555,1372105)
-- 4684660
-- 1 % 2

-- For n + m >= 10^7 + 1:
-- (19306983,7997214)
-- 27304197
-- 1 % 2

-- For n + m >= 10^8 + 1:   (Compiled, 1:40 min.)
-- (112529341,46611179)
-- 159140520
-- 1 % 2
