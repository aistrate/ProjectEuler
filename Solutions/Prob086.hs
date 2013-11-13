import Data.List (sort)
import Prob075 (allTriplets)


-- limit is: m + 2*m + 3*m = 6*m
pythagorasTriplets m = filter (\(a, b, _) -> a <= m && b <= 2 * m) $ allTriplets (6*m)


cubePairs m = let pairs = map (\(a, b, _) -> (a, b)) $ pythagorasTriplets m
                  reversedPairs = map (\(a, b) -> (b, a)) $ filter (\(_, b) -> b <= m) pairs
              in sort . filter (\(a, b) -> 2 * a >= b) $ pairs ++ reversedPairs


solutions (a, bPlusC) = if a >= bPlusC then bPlusC `div` 2
                                       else a + 1 - ((bPlusC + 1) `div` 2)


cubePaths = sum . map solutions . cubePairs


main = print $
       head . dropWhile ((< 1000000) . snd) $
       map (\m -> (m, cubePaths m)) [1..]
-- Compiled (8 sec.)
-- (1818,1000457)       (with [1800..] instead of [1..] -- 2 sec.)
