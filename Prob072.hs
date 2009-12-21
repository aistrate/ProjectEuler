import Data.List (foldl1')
import Prob069 (phi)

reducedProperFracs :: Int -> Integer
reducedProperFracs n = foldl1' (+) $ map (fromIntegral . phi) [2..n]

result = reducedProperFracs 1000000

main = print $ result
-- Compiled: 303963552391
