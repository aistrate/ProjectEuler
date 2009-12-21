module Prob065 where

import Data.Ratio
import Prob038 (digitList)
import Prob064 (continuedFrac)


partialValues :: (Int, [Int]) -> [Ratio Integer]
partialValues (f, ds) = map partialValue partialSeqs
    where digits = f : cycle ds
          partialSeqs = map (flip take $ digits) [1..]

partialValue :: [Int] -> Ratio Integer
partialValue (x:xs) | null xs   = xx
                    | otherwise = xx + inverse (partialValue xs)
    where xx = (fromIntegral x) % 1

inverse y = (denominator y) % (numerator y)

eFrac :: (Int, [Int]) 
eFrac = (2, concat $ map triplet [2, 4..])
    where triplet n = [1, n, 1]


testSqrtFrac n = --map (-sq+) . map fromRational . 
                 take 10 . partialValues $ continuedFrac n
    where sq = sqrt $ fromIntegral n

testEFrac = take 10 $ partialValues eFrac


convergent k frac = last . take k $ partialValues frac

result = sum . digitList . numerator $ convergent 100 eFrac
-- 272
