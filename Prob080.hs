import Data.Ratio
import Data.List (foldl', (\\))
import Data.Char (digitToInt)
import Prob064 (continuedFrac)


sqrtRatio :: Int -> Int -> Ratio Integer
sqrtRatio n k
        | (intSqrt n) ^ 2 == n = fromIntegral $ intSqrt n
        | otherwise            = let (f, ds) = continuedFrac n
                                     digits = map fromIntegral . take k $ f : cycle ds
                                     step d frac = d + 1 / frac
                                 in foldr1 step digits


ratioDigits :: Ratio Integer -> Int -> [Int]
ratioDigits ratio prec = let (num, denom) = (numerator ratio, denominator ratio)
                             (intPart, fracNum) = (num `div` denom, num `mod` denom)
                             divide a b = let a' = 10 * a
                                          in (fromIntegral (a' `div` b)) : divide (a' `mod` b) b
                         in take prec $ digitList intPart ++ divide fracNum denom


sqrtDigits :: Int -> Int -> [Int]
sqrtDigits n prec = ratioDigits (sqrtRatio n (2 * prec)) prec


intSqrt :: Integral a => a -> a
intSqrt = round . sqrt . fromIntegral


digitList :: Integral a => a -> [Int]
digitList = map (fromIntegral . digitToInt) . show


result = sum . map sum . map (flip sqrtDigits 100) $ [1..100] \\ map (^2) [1..10]
-- 40886


-- testing
list2Integer :: [Int] -> Integer
list2Integer = foldl' (\n d -> 10 * n + d) 0 . map fromIntegral

comp n = (take 100 $ sqrtDigits n 1000) == sqrtDigits n 100
