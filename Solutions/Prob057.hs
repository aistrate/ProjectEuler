import Data.Ratio
import Prob038 (digitList)

next :: Ratio Integer -> Ratio Integer
next x = 1 + 1 / (1 + x)

expansions = 1 : map next expansions


hasLongerNumerator r = numDigits (numerator r) > numDigits (denominator r)
    where numDigits n = length (digitList n)


main = print $
       length . filter hasLongerNumerator $ take 1001 expansions
-- 153
