import Data.Ratio
import Data.List
import Data.Char (digitToInt)
import Prob064 (continuedFrac)

sqrtRatio n k = let (f, ds) = continuedFrac n
                    digits = map fromIntegral . take k $ f : cycle ds
                    step d frac = d + 1 / frac
                in foldr1 step digits

ratioDigits ratio prec = let (num, denom) = (numerator ratio, denominator ratio)
                             (intPart, fracNum) = (num `div` denom, num `mod` denom)
                             digitList = map (fromIntegral . digitToInt) . show
                             divide a b = let a' = 10 * a
                                          in (fromIntegral (a' `div` b)) : divide (a' `mod` b) b
                         in take prec $ digitList intPart ++ divide fracNum denom

sqrtDigits n prec = ratioDigits (sqrtRatio n (2 * prec)) prec

result = sum . map sum . map (flip sqrtDigits 100) $ [1..100] \\ map (^2) [1..10]
