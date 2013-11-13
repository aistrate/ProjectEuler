import Prob003 (primeNumbers)

main = print $
       sum $ takeWhile (< 2000000) primeNumbers
-- compiled: 142913828922
