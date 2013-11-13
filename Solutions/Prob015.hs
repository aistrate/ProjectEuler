import Data.List (nub)
import Data.Ratio ((%))
import Prob003 (primeFactors)

routes :: Int -> [String]
routes 1 = ["ab", "ba"]
routes n = let rs = routes (n - 1)
               k  = 2 * (n - 1)
               inserts = [ (i, j) | i <- [0..(k-1)], j <- [1..(k-i)] ]
               newRoute a b r (i, j) = 
                    (take i r) ++ (a : (take j . drop i $ r))
                               ++ (b : (drop (i+j) r))
               newRoutes a b r = map (newRoute a b r) inserts
           in nub $ (concat $ map (newRoutes 'a' 'b') rs) ++
                    (concat $ map (newRoutes 'b' 'a') rs)

firstRoutes = map (length . routes) [1..7]      -- [1..8]
-- [2, 6, 20, 70, 252, 924, 3432, 12870]

firstFactors = map (primeFactors . fromIntegral) firstRoutes
-- [[2],[2,3],[2,2,5],[2,5,7],[2,2,3,3,7],[2,2,3,7,11],
--  [2,2,2,3,11,13],[2,3,3,5,11,13]]

firstRatios = zipWith (%) rs (1:rs)
              where rs = map fromIntegral firstRoutes
-- [2 % 1,  3 % 1,  10 % 3,  7 % 2,  18 % 5,  11 % 3,  26 % 7,  15 % 4]
-- [2 % 1,  6 % 2,  10 % 3, 14 % 4,  18 % 5,  22 % 6,  26 % 7,  30 % 8]


formula n = (2^n * product [ (2*k - 1) | k <- [2..n] ]) 
                `div` product [2..n]
formulas n = map formula [2..n]

main = print $
       formula 20
-- 137846528820
