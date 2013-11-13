import Data.List (group, sortBy)
import Timer
import Prob003 (primeFactors)


factors :: Int -> [Int]
factors 1 = [1]
factors n = map fromIntegral . primeFactors $ fromIntegral n

radical = product . map head . group . factors

sortByRadical = sortBy compRads . map (\n -> (n, radical n))
  where compRads (n1, r1) (n2, r2) = let c = compare r1 r2
                                     in if c == EQ then compare n1 n2 else c


main = printTime $
       print $
       (sortByRadical [1..100000]) !! (10000 - 1)
-- (21417,1947)
-- Time: 9.031250 sec.
