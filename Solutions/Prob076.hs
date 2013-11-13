module Prob076 (main, decomps) where


decomps :: Int -> [[Int]]
decomps n = decomps' n (n - 1)
    where decomps' d 1 = [replicate d 1]
          decomps' d t
            | d == t = [d] : decomps' d (d-1)
            | d > t  = map (t :) (decomps' (d-t) (min t (d-t))) ++
                       decomps' d (t-1)


main = print $
       length $ decomps 100
-- Compiled: 190569291      (2 min.)
