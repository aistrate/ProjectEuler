import Control.Monad
import Data.List
import Data.Word
import System.Time


minimalProdSums :: Int -> [(Int, Int)]
minimalProdSums maxN = let (ones, digitSeqs) = generateDigits maxN
                           digitSeqLength = maxN - ones
                       in filter (\(n, _) -> 2 <= n && n <= maxN)
                        . map (\(diff, prod) -> (diff + digitSeqLength, prod))
                        . map (minimumBy (\(_, prod1) (_, prod2) -> compare prod1 prod2))
                        . groupBy (\(diff1, _) (diff2, _) -> diff1 == diff2)
                        . sortBy  (\(diff1, _) (diff2, _) -> compare diff1 diff2)
                        . map (\digits -> let prod = product digits 
                                          in (prod - sum digits, prod))
                        $ digitSeqs


generateDigits :: Int -> (Int, [[Int]])
generateDigits n = (ones, generateDigits' hSeq 1 maxProduct)
    where (ones, hSeq) = halvingSeq n
          maxProduct   = 2 + sum hSeq
          generateDigits' []     _    _       = [[]]
          generateDigits' (d:ds) minN maxProd = concatMap (\k -> map (k :) $ generateDigits' ds k (maxProd `div` k))
                                                          [minN..(min d maxProd)]


halvingSeq :: Int -> (Int, [Int])
halvingSeq n = let digits = reverse . takeWhile (/= 1) $ iterate ((`div` 2) . (+1)) n
               in (n - length digits, digits)


allMinimals :: Int -> [Int]
allMinimals n = map head . group . sort . map snd $ minimalProdSums n


main = print . sum $ allMinimals 12000
-- Compiled (< 4 sec.):
-- 7587457
