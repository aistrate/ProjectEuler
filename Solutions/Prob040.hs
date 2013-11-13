import Data.Char (digitToInt)

fractionalDigits = concat $ map show [1..]

exprDigits = d 1 : d 10 : d 100 : d 1000 : d 10000 : d 100000
                 : d 1000000 : []
    where d n = digitToInt $ fractionalDigits !! (n - 1)


main = print $
       product exprDigits
-- 210
-- [1,1,5,3,7,2,1]
