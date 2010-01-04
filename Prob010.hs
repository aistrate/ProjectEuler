import Prob003 (primeNumbers)

result = sum $ takeWhile (< 2000000) primeNumbers

main = print result
-- compiled: 142913828922
