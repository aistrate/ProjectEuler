import Data.List (permutations)
import Prob035 (makeNr)

hasProperty ns = (numRange 2 4) `divBy` 2 &&
                 (numRange 3 5) `divBy` 3 &&
                 (numRange 4 6) `divBy` 5 &&
                 (numRange 5 7) `divBy` 7 &&
                 (numRange 6 8) `divBy` 11 &&
                 (numRange 7 9) `divBy` 13 &&
                 (numRange 8 10) `divBy` 17
    where numRange a b = makeNr $ takeRange a b ns
          a `divBy` b = a `mod` b == 0
          
takeRange a b = drop (a - 1) . (take b)


pandigitals = permutations [0..9]


main = print $
       sum . map makeNr $ filter hasProperty pandigitals
-- 16695334890
-- [1406357289,1460357289,4106357289,4160357289,1430952867,4130952867]
