import Prob003 (primeNumbers)


primeDecomps :: Int -> [[Int]]
primeDecomps n = primeDecomps' n (nextSmallerPrime n)
    where primeDecomps' d 1 = []
          primeDecomps' d 2 | d `mod` 2 == 0 = [replicate (d `div` 2) 2]
                            | otherwise      = []
          primeDecomps' d t
            | d == t = [d] : primeDecomps' d (nextSmallerPrime d)
            | d > t  = map (t :) (primeDecomps' (d-t) 
                                    (min t (nextSmallerPrime (d-t+1))))
                       ++ primeDecomps' d (nextSmallerPrime t)


nextSmallerPrime :: Int -> Int
nextSmallerPrime 2 = 1
nextSmallerPrime k = fromIntegral . last 
                        $ takeWhile (< (fromIntegral k)) primeNumbers


main = print $
       head . dropWhile ((<= 5000) . snd) $
       map (\n -> (n, length $ primeDecomps n)) [4..]
-- (71,5006)
