import Data.List (foldl1')

-- the fraction a/b is coded as (a, b)

-- max numerator k so that k/n < a/b
maxNumerator :: (Int, Int) -> Int -> Int
maxNumerator (a, b) n = (a * n) `div` b

-- RPF = Reduced Proper Fraction
maxRpfNumerator :: (Int, Int) -> Int -> Int
maxRpfNumerator (a, b) n | b == n = a - 1
maxRpfNumerator frac n   = head $ filter (relPrimes n) [m,m-1..0]
    where m = maxNumerator frac n
          relPrimes a b = gcd a b == 1

maxReducedProperFraction :: (Int, Int) -> [Int] -> (Int, Int)
maxReducedProperFraction frac = foldl1' maxFrac . map maxRpf
    where maxRpf n = (maxRpfNumerator frac n, n)
          maxFrac p q | toFloat p > toFloat q = p
                      | otherwise             = q

toFloat :: (Int, Int) -> Double
toFloat (a, b) = fromIntegral a / fromIntegral b


main = print $
       fst $ maxReducedProperFraction (3, 7) [3..1000000]
-- 428570
-- (428570,999997)
