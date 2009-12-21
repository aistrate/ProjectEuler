import Data.Array
import Data.List (find)


maxN = 4000

-- let ~x = parts k n in x
partArr = array ((1, 1), (maxN, maxN)) 
                $ concatMap (\k -> map (\n -> ((k, n), parts k n)) 
                                       [1..maxN]) 
                            [1..maxN]
    where parts k n | k > n     = 0
                    | k == n    = 1
                    | otherwise = partArr ! (k+1, n) +
                                  partArr ! (k, n-k)

partitions n = partArr ! (1, n)


{-partArr = array ((1, 1), (maxN, maxN)) 
                $ concatMap (\k -> map (\n -> ((k, n), \_ -> parts k n)) 
                                       [1..maxN]) 
                            [1..maxN]
    where parts k n | k > n     = 0
                    | k == n    = 1
                    | otherwise = (partArr ! (k+1, n)) () +
                                  (partArr ! (k, n-k)) ()

partitions n = (partArr ! (1, n)) ()-}


main = do let pairs = filter (\(_, part) -> part `mod` 10000 == 0) 
                        $ map (\n -> (n, partitions n)) [1..maxN]
          mapM_ print pairs
          print $ length pairs


-- Most trailing zeros up to 4000 (~ 20 min., compiled):
-- 	(599,435350207840317348270000)
-- 	(776,1973678121921532286407950000)
-- 	(1949,1111405941905549479818145590739116367242780000)
-- 	(2499,2799278702412287477405614444445747930301938442180000)
