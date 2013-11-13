import qualified Data.Map as M

maxLimit = 2000000000

triangle n = (n * (n + 1)) `div` 2
triangleList = map triangle [1..]

pentagonal n = (n * (3 * n - 1)) `div` 2
pentagonals = makeMapWith pentagonal
isPentagonal n = (M.lookup n pentagonals) /= Nothing

hexagonal n = n * (2 * n - 1)
hexagonals = makeMapWith hexagonal
isHexagonal n = (M.lookup n hexagonals) /= Nothing

makeMapWith formula = M.fromList $ takeWhile ((<= maxLimit) . fst) 
                                 $ map (\n -> (formula n, n)) [1..]


isPentaHexa n = isPentagonal n && isHexagonal n


main = print $
       head $ filter isPentaHexa $ takeWhile (<= maxLimit) $
       dropWhile (<= 40755) triangleList
-- 1533776805
