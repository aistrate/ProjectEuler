import Data.List (sort, group)
import Prob038 (digitList)

sameDigitMultiples n = (length . group $ map orderedDigits multiples) == 1
    where multiples = map (n *) [1..6]
          orderedDigits = sort . digitList


main = print $
       head $ filter sameDigitMultiples [1..]
-- 142857
