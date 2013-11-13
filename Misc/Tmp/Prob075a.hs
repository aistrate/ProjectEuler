import Data.List
import Data.Array
import Data.Map as M hiding (map)
import Data.Maybe
import Control.Monad


limit :: Integer
limit = 50000


squares :: Array Integer Integer
squares = array (1, limit `div` 2) $ 
                map (\i -> (i, i ^ 2)) [1..limit `div` 2]

binarySearch :: (Integral i, Ix i, Ord e) => e -> Array i e -> Maybe i
binarySearch e arr = search e (bounds arr)
    where search e (lo, hi) 
            | lo >= hi && e /= arr Data.Array.! lo = Nothing
            | otherwise = let mid = (lo + hi) `div` 2 in
                          case compare e (arr Data.Array.! mid) of
                              EQ -> Just mid
                              LT -> search e (lo, mid - 1)
                              _  -> search e (mid + 1, hi)

testBinSrc = map (flip binarySearch squares)
                 [1, 2, 3, 4, hi ^ 2, hi ^ 2 - 1,
                  23155, 1231^2, 511^2, 511^2 - 1, 144, 2500,
                  (hi `div` 2) ^ 2, (hi `div` 2) ^ 2 - 1,
                  (hi `div` 2) ^ 2 + 1,
                  (23 * hi `div` 64 + 17) ^ 2]
    where (lo, hi) = bounds squares


squaresMap = M.fromList $ map (\i -> (i ^ 2, i)) [1..limit `div` 2]


triplets :: [(Integer, Integer, Integer)]
triplets = do a <- [1..(limit `div` 3)]
              b <- [a + 1..((limit - a) `div` 2)]
              let c2 = a ^ 2 + b ^ 2
              guard $ member c2 squaresMap
              let c = squaresMap M.! c2
              guard $ a + b + c <= limit
              return (a, b, c)


result0 = Data.List.filter ((== 1) . length . snd)
        . map (\g -> (fst $ head g, map snd g))
        . groupBy (\a b -> (fst a) == (fst b))
        . sortBy (\a b -> compare (fst a) (fst b))
        . map (\t@(a, b, c) -> (a + b + c, t)) 
        $ triplets

result = map head
       . Data.List.filter ((== 1) . length)
       . group . sort
       . map (\(a, b, c) -> a + b + c) 
       $ triplets


main = do -- print result
          print $ length result
-- to 2000:  222
-- to 20000: 2209
-- to 50000: 5502


-- All triplets:
--      to 2000:  744
--      to 20000: 10689
