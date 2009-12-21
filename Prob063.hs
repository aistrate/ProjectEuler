digitCount = (+1) . truncate . logBase 10 . fromIntegral

powers n = [ (i, n ^ i) | i <- [1..] ]

eqPowers = map snd . takeWhile digCountEqPow . powers
    where digCountEqPow (a, b) = digitCount b == a

result = length . concat . takeWhile (not . null) $ map eqPowers [1..]
-- 49
