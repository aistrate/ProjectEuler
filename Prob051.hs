import qualified Data.Map as M
import Data.List (sortBy, groupBy, maximumBy, nub)
import Data.Maybe (catMaybes)
import Prob003 (primeNumbers)
import Prob012 (combinations)
import Prob035 (makeNr)
import Prob038 (digitList)

numDigits = 6
minFamily = 8

minLimit = 10 ^ (numDigits - 1)  -- 100000
maxLimit = 10 ^ numDigits - 1    -- 999999
primesList = primesRange minLimit maxLimit

primesRange a b = takeWhile (<= b) $ dropWhile (< a) primeNumbers


allCombinations = concat [ combinations k positions | 
                           k <- reverse [1..numDigits-1] ]
                  where positions = [1..numDigits]


createMask comb = map (`elem` comb) [1..numDigits]

applyMask mask n = catMaybes $ zipWith maskDigit mask (digitList n)
    where maskDigit dMask d = if dMask then Just d else Nothing


groupByMask mask ns = map (map snd) $ 
                      filter ((>= minFamily) . length) grouped
    where grouped = groupBy eqMasked $ sortBy compMasked masked
          masked = map (\n -> (makeNr (applyMask mask n), n)) ns
          compMasked a b = compare (fst a) (fst b)
          eqMasked a b   = compMasked a b == EQ

filterEqualDigits mask ns = filter hasEqualDigits ns
    where hasEqualDigits n = (length . nub $ applyMask mask n) == 1

findFamilies mask ns = 
    groupByMask mask $ filterEqualDigits (map not mask) ns


allFamilies ns = sortBy compFamilies families
    where families = concat $ map familiesPerComb allCombinations
          familiesPerComb comb = findFamilies (createMask comb) ns
          compFamilies f g = compare (head f) (head g)


result = allFamilies primesList
-- [[121313,222323,323333,424343,525353,626363,828383,929393]]


main = print result
