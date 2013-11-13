decomps :: Int -> [[Int]]
decomps n = decomps' [] n (n - 1)
    where decomps' cache d 1 = [replicate d 1]
          decomps' cache d t
            | d == t = let c = [d] : decomps' d (d-1) in
                       (c, 
            | d > t  = map (t :) (decomps' (d-t) (min t (d-t))) ++
                       decomps' d (t-1)


main = print . length $ decomps 100
