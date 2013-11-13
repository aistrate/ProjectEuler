import Data.Maybe (isNothing)


decomps :: Int -> [[Int]]
decomps n = snd $ decomps' [] n (n - 1)
    where decomps' cache d 1 = (cache, [replicate d 1])
          decomps' cache d t = 
            case lookup (d, t) cache of
              Just ds -> (cache, ds)
              Nothing -> 
                         case True of
                              _ | d == t -> let (cache', res) = decomps' cache d (d-1)
                                                res' = [d] : res
                                                cache'' = addToCache cache' ((d, t), res')
                                            in (cache'', res')
                                | d > t  -> let (cache', res')   = decomps' cache (d-t) 
                                                                                  (min t (d-t))
                                                (cache'', res'') = decomps' cache' d (t-1)
                                                res''' = map (t :) res' ++ res''
                                                cache''' = addToCache cache'' ((d, t), res''')
                                            in (cache''', res''')
          addToCache cache (i, e) = if isNothing (lookup i cache)
                                        then (i, e) : cache
                                        else cache


main = print . length $ decomps 90
-- Compiled: 190569291      (2 min.)
