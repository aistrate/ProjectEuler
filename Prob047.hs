import Data.List (nub, groupBy)
import Prob003 (primeFactors)

distinctPrimeFactors = length . nub . primeFactors

dpfCounts = map (\n -> (n, distinctPrimeFactors n)) [2..]

consecutiveDpfCounts = groupBy sameDpf dpfCounts
    where sameDpf a b = snd a == snd b


findConsecutive len dpfCnt = head $ filter goodGroup consecutiveDpfCounts
    where goodGroup gr = length gr >= len && snd (head gr) == dpfCnt
-- [(134043,4),(134044,4),(134045,4),(134046,4)]


main = print $
       fst . head $ findConsecutive 4 4
-- 134043
