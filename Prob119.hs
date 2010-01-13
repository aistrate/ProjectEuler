import Data.Char (ord)
import TestHelper


sorted ls = let minH = minimum $ map head ls
                newLs = map (\l -> if head l == minH then tail l else l) ls
            in minH : sorted newLs

powers = map (\n -> map (^n) [2..]) [2..20]

sumDigits :: Integer -> Integer
sumDigits n = sum . map (\c -> fromIntegral(ord c - ord '0')) $ show n

logInt :: Integer -> Integer -> Integer
logInt b n = truncate $ logBase (fromIntegral b) (fromIntegral n)

baseExp n = let b = sumDigits n
                e = logInt b n
            in (b, e)

hasProperty n = let (b, e) = baseExp n
                in b^e == n

propertyNums = dropWhile (< 10) . filter hasProperty $ sorted powers


main = let n = last . take 30 $ propertyNums
           (b, e) = baseExp n
       in printTime $ print (n, b, e)
