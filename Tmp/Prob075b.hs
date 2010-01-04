import Data.List
import Data.Map as M hiding (map)
import Control.Monad


limit :: Integer
limit = 2000

squares        = M.fromList $ map (\i -> (i, i ^ 2)) [1..limit `div` 2]
squaresInverse = M.fromList $ map (\i -> (i ^ 2, i)) [1..limit `div` 2]


triplets :: [(Integer, Integer, Integer)]
triplets = do a <- [1..(limit `div` 3)]
              b <- [a + 1..((limit - a) `div` 2)]
              let c2 = squares M.! a + squares M.! b
              guard $ member c2 squaresInverse
              let c = squaresInverse M.! c2
              guard $ a + b + c <= limit
              return (a, b, c)


result = map head
       . Data.List.filter ((== 1) . length)
       . group . sort
       . map (\(a, b, c) -> a + b + c) 
       $ triplets


main = print $ length result
-- to 2000:  222
-- to 20000: 2209   (22 sec.)
-- to 50000: 5502   (142 sec.)
