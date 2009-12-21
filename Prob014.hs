import Data.Map (fromList, insert, (!))
import Data.List (foldl')

nextInChain :: Integer -> Integer
nextInChain n | n `mod` 2 == 0 = n `div` 2
              | otherwise      = 3 * n + 1

-- not used
chain :: Integer -> [Integer]
chain 1 = [1]
chain x = x : chain (nextInChain x)


chainLengths = chainLengths' 2 (fromList [(1, 1)])
   where chainLengths' n ls = let len = chainLength n
                              in (n, len) : chainLengths' (n + 1) (insert n len ls)
            where chainLength k | k < n     = ls ! k
                                | otherwise = 1 + chainLength (nextInChain k)


longestChain n = foldl' maxP (1, 1) (take n chainLengths)
                 where maxP a b | snd b > snd a = b
                                | otherwise     = a


result = longestChain (1000000 - 1)
-- (837799,525)

main = print result

-- filter (\p -> 500 <= snd p) (take 1000000 chainLengths)
