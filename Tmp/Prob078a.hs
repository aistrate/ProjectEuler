import Data.Maybe


{-decompCount :: Integer -> Integer
decompCount n = decompCount' n n
    where decompCount' d 1 = 1
          decompCount' d t
            | d == t = 1 + decompCount' d (d-1)
            | d > t  = (decompCount' (d-t) (min t (d-t))) +
                       (decompCount' d (t-1))-}


decompCount :: Integer -> Integer
decompCount n = snd $ decompCount' [] n n
    where decompCount' cache d 1 = (cache, 1)
          decompCount' cache d t
            | d == t = case lookup d cache of
                         Just c -> (cache, c)
                         Nothing ->
                            let (cache', cnt) = decompCount' cache d (d-1)
                            in ((d, 1 + cnt) : cache', 1 + cnt)
            | d > t  = let (cache', cnt')   = decompCount' cache (d-t) (min t (d-t))
                           (cache'', cnt'') = decompCount' cache' d (t-1)
                       in (cache'', cnt' + cnt'')


main = mapM_ print $ map (\n -> (n, decompCount n)) [100..120]
