import Data.List (sort, foldl1')
import Prob038 (digitList)
import Prob069 (phi, phiFraction)


isDigitPerm a b = digs a == digs b
    where digs = sort . digitList

phiPerms n = filter (\k -> isDigitPerm k (phi k)) [2..n]

minPhiPerm n = fst $ foldl1' minPair phiPairs
    where phiPairs = map (\k -> (k, phiFraction k)) (phiPerms n)
          minPair p q | snd p <= snd q = p
                      | otherwise      = q


main = print $
       minPhiPerm 10000000
-- 8319823  (Compiled - more than 12 min.)
--      -> 8313928 (1.0007090511248113)
-- 783169 (less than 1 mil.)
