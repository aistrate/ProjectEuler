import Data.List (nub)
import Timer


squareSum :: Integer -> Integer -> Integer
squareSum k n = sum $ map (^2) [n..n+k-1]

consSquareSums k n = takeWhile (< n) $ map (squareSum k) [1..]

allConsSquareSums n = concat . takeWhile (not . null) $ map (flip consSquareSums n) [2..]

isPalindrome n = let s = show n in s == reverse s


main = printTime . print .
       sum . nub . filter isPalindrome . allConsSquareSums $ 10^8
-- 2906969179
-- Time: 6.421875 sec.
-- (compiled)
