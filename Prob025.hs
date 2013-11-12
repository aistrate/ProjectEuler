import Data.List

fibonacciNums = 1 : 1 : zipWith (+) fibonacciNums (tail fibonacciNums)

main = print $
       (1+) . head $ findIndices ((>= 1000) . length . show) fibonacciNums
-- 4782
