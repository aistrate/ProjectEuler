import Data.List

lexPerm = foldl' addDigit 0
          where addDigit s d = 10 * s + d

result = lexPerm $ (sort $ permutations [0..9]) !! (1000000 - 1)
-- 2783915460
