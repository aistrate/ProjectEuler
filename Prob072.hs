import Data.List (foldl1')
import Prob069 (phi)

reducedProperFracs :: Int -> Integer
reducedProperFracs n = foldl1' (+) $ map (fromIntegral . phi) [2..n]


main = print $
       reducedProperFracs 1000000
-- Compiled: 303963552391
