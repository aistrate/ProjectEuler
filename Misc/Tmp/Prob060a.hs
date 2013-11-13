import Data.List
import qualified Data.Map as M
import Prob003 (primeNumbers, primeFactors)
import Prob012 (combinations)
import Prob035 (makeNr)
import Prob038 (digitList)

maxLimit = 1999
primesList = primesRange 2 maxLimit
primesRange a b = takeWhile (<= b) $ dropWhile (< a) primeNumbers

maxConcat = makeNr $ digitList maxLimit ++ digitList maxLimit
primes = M.fromList . map (\p -> (p, True)) 
             $ primesList ++ primeConcatenations
isPrime n = (M.lookup n primes) /= Nothing
loadMap = length $ M.toAscList primes   -- for testing

primeConcatenations = [ c | a <- primesList, b <- primesList, 
                            let c = concatPrimes a b,
                            c > maxLimit, isPrimeConcat c ]
    where isPrimeConcat n = not (any (`divisorOf` n) candidateFactors)
          candidateFactors = primesRange 2 maxRoot
          maxRoot = truncate . sqrt $ fromIntegral maxConcat
          a `divisorOf` b = b `mod` a == 0


concatPrimes p q = makeNr $ digitList p ++ digitList q

isGoodPair p q = isPrime (concatPrimes p q) && 
                 isPrime (concatPrimes q p)


combine :: [[Integer]] -> [Integer] -> [[Integer]]
combine keyList primes = nub [ sort (p:ks) | ks <- keyList, p <- primes,
                                             ks `allPairWith` p ]
    where ks `allPairWith` p = (not (p `elem` ks)) && 
                               (all (isGoodPair p) ks)


primeConcatLists 1 = map (:[]) primesList
primeConcatLists n = combine (primeConcatLists (n - 1)) primesList


result = minimum . map sum $ primeConcatLists 5
-- [[3,7,109,673],[23,311,677,827]]


main = print $ primeConcatLists 5
-- Compiled: [[13,5197,5701,6733,8389]]
