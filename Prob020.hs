import Data.Char (digitToInt)

fact :: Integer -> Integer
fact n = fact' n 1
         where fact' 1 a = a
               fact' n a = fact' (n - 1) (a * n)


result = sum . map digitToInt . show $ fact 100
-- 648
