import Data.Array
import Prob021 (properDivisors)

isAbundant a = sum (properDivisors a) > a


limit = 28123

isAbundantArr = listArray (1, limit) $ map isAbundant [1..limit]

abundants = filter (isAbundantArr !) $ indices isAbundantArr


terms n = filter (isAbundantArr !) . map (n -) . filter (<= mid) $ abundants
          where mid = n `div` 2

expressible = not . null . terms

nonExpressibles = filter (not . expressible) [1..limit]


main = print $
       sum nonExpressibles
-- 4179871


-- Checking
counts = map (length . terms) [1..limit]

sums = map fstSum [1..limit]
    where fstSum n | null (terms n) = (0, 0)
                   | otherwise      = (head (terms n), n - head (terms n))
