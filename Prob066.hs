import Data.Ratio
import Data.List ((\\), maximumBy)
import Prob064 (continuedFrac)
import Prob065 (partialValues)


candidates d = map pair . tail . partialValues $ continuedFrac d
    where pair q = (numerator q, denominator q)

isDiophantine :: Int -> (Integer, Integer) -> Bool
isDiophantine d (x, y) = (x*x - (fromIntegral d)*y*y) == 1

solveFor d = head . filter (isDiophantine d) $ candidates d


dValues n = [1..n] \\ (takeWhile (<= n) [ i * i | i <- [1..] ])

solutions :: Int -> [(Int, (Integer, Integer))]
solutions = map sol . dValues
    where sol d = (d, solveFor d)

maxD = fst . maximumBy compX . solutions
    where compX p q = compare (getX p) (getX q)
          getX = fst . snd


main = print $
       maxD 1000
-- 661
--      x = 16421658242965910275055840472270471049
--      y = 638728478116949861246791167518480580
