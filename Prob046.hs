import qualified Data.Map as M
import Prob003 (primeNumbers)

maxLimit = 10000
primes = M.fromList . map (\p -> (p, True)) 
            $ takeWhile (<= maxLimit) primeNumbers

isPrime n = (M.lookup n primes) /= Nothing


oddComposites = filter (not . isPrime) [9,11..]


doubleSquares = map doubleSquare [1..]
                where doubleSquare n = 2 * (n * n)

canFindPrime n = any isPrime . map (n -) $ takeWhile (< n) doubleSquares


result = head . filter (not . canFindPrime) 
              $ takeWhile (<= maxLimit) oddComposites
-- 5777
