import Prob003 (primeNumbers)

main = print $
       last (take 10001 primeNumbers)
-- 104743
