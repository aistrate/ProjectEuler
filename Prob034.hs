import Data.List (inits)
import Data.Array
import Data.Char (digitToInt)

factorials = listArray (0, 9) $ (1 :) $ map product . tail $ inits [1..9]


isCuriousNumber n = n == sum (map (factorials !) digits)
                    where digits = map digitToInt (show n)

curiousNumbers n = filter isCuriousNumber [10..n]


-- 9999999 has only 7 nines; 7 * 9! = 7 * 362880 = 2540160 << 9999999
result = sum $ curiousNumbers 2600000
-- [145,40585]
-- 40730
