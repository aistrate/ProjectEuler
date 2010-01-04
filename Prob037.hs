import Data.Char (digitToInt)
import Data.List (tails, nub, (\\))
import Data.Array
import Prob003 (primeNumbers)
import Prob027 (isPrimeList)
import Prob035 (makeNr)

maxPrime = 1000000
-- 0 and 1 are NOT primes
primes = listArray (0, maxPrime) (False : False : tail isPrimeList)


truncs n = nub . map makeNr $ leftTrunc digits ++ rightTrunc digits
    where leftTrunc = init . tail . tails
          rightTrunc = map reverse . leftTrunc . reverse
          digits = map (fromIntegral . digitToInt) $ show n


isTruncatable p = all (primes !) $ truncs p

allTruncatable n = filter isTruncatable $ 
                   takeWhile (<= n) $ dropWhile (< 11) primeNumbers


result = sum $ allTruncatable 1000000
-- [23,37,53,73,313,317,373,797,3137,3797,739397]
-- 748317
