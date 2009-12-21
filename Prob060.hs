import Data.List
import qualified Data.Map as M
import Prob003 (primeNumbers, primeFactors)
import Prob012 (combinations)
import Prob035 (makeNr)
import Prob038 (digitList)

maxLimit = 9999
primesList = primesRange 2 maxLimit
primesRange a b = takeWhile (<= b) $ dropWhile (< a) primeNumbers

goodPairs = M.fromList $ map (\pair -> (pair, True)) primeConcatenations
primeConcatenations = [ (a, b) | a <- primesList,
                                 b <- primesList, a < b,
                                 isPrime (concatPrimes a b),
                                 isPrime (concatPrimes b a) ]
    where isPrime n = null . tail $ primeFactors n
          concatPrimes p q = makeNr $ digitList p ++ digitList q
loadMap = length $ M.toAscList goodPairs   -- for testing


isGoodPair a b = (M.lookup pair goodPairs) /= Nothing
    where pair = if a < b then (a, b) else (b, a)


combine :: [[Integer]] -> [Integer] -> [[Integer]]
combine keyList primes = nub [ sort (p:ks) | ks <- keyList, p <- primes,
                                             ks `allPairWith` p ]
    where ks `allPairWith` p = (not (p `elem` ks)) && 
                               (all (isGoodPair p) ks)


primeConcatLists 1 = map (:[]) primesList
primeConcatLists n = combine (primeConcatLists (n - 1)) primesList
-- [[3,7,109,673], [11,23,743,1871], [11,239,1049,1847],
--  [11,239,1091,1847], [23,311,677,827], [23,677,827,1871]]


result = minimum . map sum $ primeConcatLists 5

main = print $ primeConcatLists 5
-- Compiled (< 1 min.): [[13,5197,5701,6733,8389]]
-- 26033
