import Data.Array
import Data.List
import Control.Monad
import Prob003 (primeNumbers)


maxElement = 1000000

-- Prime factors
primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n (map fromIntegral primeNumbers)
    where primeFactors' 1 _     = []
          primeFactors' n (p:_)
              | p * p > n       = [n]
          primeFactors' n primes@(p:ps)
              | n `mod` p == 0  = p : (primeFactorsArr ! (n `div` p))
              | otherwise       = primeFactors' n ps

primeFactorsArr :: Array Int [Int]
primeFactorsArr = listArray (1, maxElement) $ map primeFactors [1..maxElement]


-- Proper divisors
properDivisors :: Int -> [Int]
properDivisors n = let prFactorGroups = group $ primeFactorsArr ! n
                       prFactors      = map head prFactorGroups
                       maxPowers      = map length prFactorGroups
                       allPowers      = sequence $ map (\k -> [0..k]) maxPowers
                   in init . sort $ map (product . zipWith (^) prFactors) allPowers


-- Amicable chains
next = sum . properDivisors

nextArr :: Array Int Int
nextArr = listArray (1, maxElement) $ map next [1..maxElement]

chain = iterate (nextArr !)


boundedChain n = takeWhile (\v -> 0 < v && v <= maxElement) 
               $ chain n

amicableChain = amicableChain' []
    where amicableChain' _    []                     = []
          amicableChain' seen (n:ns) | n `elem` seen = reverse seen
                                     | otherwise     = amicableChain' (n:seen) ns

boundedAmicableChain = amicableChain . boundedChain

boundedAmicableChains ns = filter (\cs -> head cs == nextArr ! (last cs))
                         . filter (not . null)
                         $ map boundedAmicableChain ns


maxLengthChains ns = map (boundedAmicableChain . fst)
                   . last
                   . groupBy (\p q -> snd p == snd q)
                   . sortBy (\p q -> compare (snd p) (snd q))
                   . map (\ns -> (head ns, length ns))
                   $ boundedAmicableChains ns

uniqueMaxLengthChains ns = map (boundedAmicableChain . head)
                         . nub . map sort
                         $ maxLengthChains ns


main = do let interval = [1..maxElement]
          mapM_ print $ boundedAmicableChains interval
          let longestChains = uniqueMaxLengthChains interval
          putStrLn "\nLongest chains:"
          mapM_ print longestChains
          putStr "\nLength: "
          print . length $ head longestChains
          putStr "\nAnswer: "
          print . minimum $ head longestChains
-- Compiled: 27 sec
-- ...
-- Longest chains:
-- [14316,19116,31704,47616,83328,177792,295488,629072,589786,294896,358336,418904,
--  366556,274924,275444,243760,376736,381028,285778,152990,122410,97946,48976,45946,
--  22976,22744,19916,17716]
-- Length: 28
-- Answer: 14316
