import Prob003 (primeNumbers)
import Timer


calcS :: [Integer] -> Int -> Integer
calcS ns d = let c = head $ show d
                 rs = map (\n -> (n, length . filter (== c) $ show n)) ns
                 m = maximum $ map snd rs
             in sum . map fst $ filter ((m ==) . snd) rs

primesInterval :: Integer -> Integer -> [Integer]
primesInterval m n = filter (\k -> not $ any (`divOf` k)
                                   (takeWhile (<= (intSqrt n)) primeNumbers)) [m..n]
  where a `divOf` b = b `mod` a == 0
        intSqrt n   = round . sqrt $ fromIntegral n


result = sum $ map (calcS (primesInterval 100000 1000000)) [0..9]

main = printTime $ print result
