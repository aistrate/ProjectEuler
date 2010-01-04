import Data.Array

-- u(n) = a0 + a1(n - 1) + a2(n - 1)(n - 2) + ... + a10(n - 1)(n - 2)...(n - 10)

calcU n = sum $ map ((-n)^) [0..10]

calcA 0 = calcU 1
calcA k = (calcU (k + 1) - calcFIT k) `div` (product [1..k])

a = listArray (0, 10) $ map calcA [0..10]

calcOP k n = sum [ (a ! i) * (product (1 : [ n - j | j <- [1..i] ])) | i <- [0..k-1] ]
calcFIT k = calcOP k (k + 1)

result = sum $ map calcFIT [1..10]
-- 37076114526
