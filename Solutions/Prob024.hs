import Data.List

lexPerm = foldl' addDigit 0
          where addDigit s d = 10 * s + d

main = print $
       lexPerm $ (sort $ permutations [0..9]) !! (1000000 - 1)
-- 2783915460
