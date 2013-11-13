import Data.Array
import Data.List (sortBy)

pentagonal :: Int -> Int
pentagonal n = (n * (3 * n - 1)) `div` 2


maxLimit = 10000

pentagonals :: Array Int Int
pentagonals = listArray (1, maxLimit) $ map pentagonal [1..]

arePentagonals :: Array Int Bool
arePentagonals = listArray (1, pentagonal maxLimit) (cycle [False])
                    // map (\n -> (pentagonal n, True)) [1..maxLimit]


isPentagonalPair :: Int -> Int -> Bool
isPentagonalPair j k = arePentagonals ! (pk + pj) && 
                       arePentagonals ! (pk - pj)
                       where pj = pentagonals ! j
                             pk = pentagonals ! k


pentagonalPairs max = [ (j, k) | j <- [1..max], k <- [j+1..max],
                                 isPentagonalPair j k ]
-- [(1020,2167)]
-- pj = 1560090, pk = 7042750
-- pk + pj = 8602840  == pentagonal 2395
-- pk - pj = 5482660  == pentagonal 1912


main = print $
       diff . head $ sortBy compPairs (pentagonalPairs (maxLimit `div` 2))
         where diff (j, k) = (pentagonals ! k) - (pentagonals ! j)
               compPairs p q = compare (diff p) (diff q)
-- 5482660
