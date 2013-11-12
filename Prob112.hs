import Data.List (sort)
import Timer


isBouncy :: Integer -> Bool
isBouncy n = let r = show n
                 s = sort r
             in r /= s && r /= reverse s

bouncyNumbers = filter isBouncy [1..]

proportions = zipWith (\k n -> (k, n, fromIntegral k / fromIntegral n))
                      [1..] bouncyNumbers


main = printTime $
       print $
       head $ filter (\(_, _, p) -> p == 0.99) proportions
-- (1571130,1587000,0.99)
-- Time: 13.094756 sec.
