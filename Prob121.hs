import Data.Ratio
import Data.List ((\\))
import Timer
import Prob012 (combinations)


probabilities = map (1 %) [2..]

probability n k = let ps = take n probabilities
                      cs = combinations k ps
                      anti c = map ((1 % 1) -) (ps \\ c)
                      probForCase c = product (c ++ anti c)
              in sum $ map probForCase cs

totalProbability n = sum $ map (probability n) [((n `div` 2) + 1)..n]

maxPrize n = let p = totalProbability n
             in (denominator p) `div` (numerator p)


main = printTime . print $ maxPrize 15
-- 2269
-- Time: 16.953125 sec.
-- (compiled)
