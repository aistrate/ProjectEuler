import Data.Char (ord)
import Data.List (sort)
import Timer


sumDigits :: Integer -> Integer
sumDigits n = sum . map (\c -> fromIntegral(ord c - ord '0')) $ show n

hasProperty (n, b, _) = n > 10 && sumDigits n == b

powers = [ (n, b, e) | b <- [2..200], e <- [1..20], let n = b^e ]

propertyNums = sort . filter hasProperty $ powers


main = printTime . print . last . take 30 $ propertyNums
-- (248155780267521,63,8)
-- Time: 0.218750 sec.
