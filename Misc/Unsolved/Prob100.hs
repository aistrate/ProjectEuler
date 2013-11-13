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


-- 8n^2 - 8n + 1 == (2t - 1)^2
-- 8n^2 - 8n == 4t(t - 1)
-- 2n(n - 1) == t(t - 1)

-- Ex:
-- 420 = 2 * 2 * 3 * 5 * 7
--     = 2 * (3 * 5) * (2 * 7) = 2 * 15 * (15 - 1)
--     = (3 * 7) * (2 * 2 * 5) = 21 * (21 - 1)


import Data.Ratio


squareDeltaNs minN = let n2s = map (\n -> n * (n - 1)) [minN..]
                         n2ds = map (\n -> 2 * n * (n - 1)) [minN..]
                         goodN2s = equalElems n2s n2ds
                         calcN n = (1 + intSqrt (1 + 4 * (n `div` 2))) `div` 2
                     in map calcN goodN2s


{-equalElems :: [Integer] -> [Integer] -> [Integer]
equalElems (x:xs) (y:ys) | x == y    = x : equalElems xs ys
                         | x <  y    = equalElems (dropWhile (< y) xs) (y:ys)
                         | otherwise = equalElems (x:xs) (dropWhile (< x) ys)-}

equalElems :: [Integer] -> [Integer] -> [Integer]
equalElems (x:xs) (y:ys) | x == y    = x : equalElems xs ys
                         | x <  y    = equalElems xs (y:ys)
                         | otherwise = equalElems (x:xs) ys


intSqrt :: Integral a => a -> a
intSqrt = truncate . sqrt . fromIntegral


calcMinN :: Integer -> Integer
calcMinN minNPlusM = ceiling ((sqrt (2 * ((2 * (fromIntegral minNPlusM) - 1)^2 + 1)) + 2) / 4)


calcDelta :: Integer -> Integer
calcDelta n = 8*n^2 - 8*n + 1

nmPairs2 :: Integer -> [(Integer, Integer)]
nmPairs2 minN = let nmPair n = let delta = calcDelta n
                                   m = (intSqrt delta + 1) `div` 2 - n
                               in (n, m)
                in map nmPair . squareDeltaNs $ minN


probability :: (Integer, Integer) -> Rational
probability (n, m) = let nn = n % 1
                         mm = m % 1
                     in nn / (nn + mm) * (nn - 1) / (nn + mm - 1)


main = let printPair p@(n, m) = do print p
                                   print $ n + m
                                   print $ probability p
                                   putStrLn ""
       in mapM_ printPair . take 1 . nmPairs2 . calcMinN $ 10^6 + 1


-- For n + m >= 10^6 + 1:   (Compiled, 3 s)
-- (3312555,1372105)
-- 4684660
-- 1 % 2

-- For n + m >= 10^7 + 1:   (Compiled, 18 s)
-- (19306983,7997214)
-- 27304197
-- 1 % 2

-- For n + m >= 10^8 + 1:   (Compiled, 70 s)
-- (112529341,46611179)
-- 159140520
-- 1 % 2
