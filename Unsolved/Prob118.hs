import Data.Char (ord)
import Data.List (nub)
import Timer
import Prob003


digits :: Integer -> [Int]
digits n = map (\c -> fromIntegral(ord c - ord '0')) $ show n

candidatePrimes = filter (\ds -> ds == nub ds). map digits . takeWhile (< 987654321) 
                  $ primeNumbers
