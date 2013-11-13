import Data.List (sortBy, (\\))
import Timer
import Prob003


isPrime n = length (take 2 $ primeFactors n) == 1

hasProperty n = let n2 = n^2
                in all (\(k, b) -> isPrime (n2 + k) == b) pattern
  where pattern = let cs = map (\n -> (n, True)) [1, 3, 7, 9, 13, 27]
                      ds = map (\n -> (n, False)) [5, 11]
                  in (sortBy (\a b -> compare (fst a) (fst b)) (cs ++ ds)) ++
                     map (\n -> (n, False)) [15,17..25]

main = printTime . print $ filter hasProperty [2,4..1000000]
