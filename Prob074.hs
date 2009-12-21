import Data.Array


digits :: Int -> [Int]
digits n | n < 10    = [n]      -- we assume n is non-negative
         | otherwise = digits (n `div` 10) ++ [n `mod` 10]


factorial 0 = 1
factorial n = n * factorial (n - 1)

factorials :: [Int]
factorials = map factorial [0..9]


nextSum = sum . map (factorials !!) . digits

chain n = n : chain (nextSum n)


longestNonrep xs = reverse $ longest [] xs
    where longest ys zs | head zs `elem` ys = ys
                        | otherwise = longest (head zs : ys) (tail zs)


nrLenPair n = (n, length . longestNonrep . chain $ n)

chainsThru :: Int -> Array Int Int
chainsThru n = array (1, n) $ map nrLenPair [1..n]


maxMemo = 9999
firstChains = chainsThru maxMemo

-- this works like (length . longestNonrep . chain), but with memoization
chainLengthOpt :: Int -> Int
chainLengthOpt n = chainLengthOpt' 0 [] (chain n)
    where chainLengthOpt' len ys zs
                | head zs <= maxMemo = len + firstChains ! (head zs)
                | head zs `elem` ys  = len
                | otherwise          = chainLengthOpt' (len + 1)
                                            (head zs : ys) (tail zs)


chainStarts = filter ((==60) . snd) . map (\k -> (k, chainLengthOpt k)) $
                   [1..1000000]

result = length chainStarts
-- 402
