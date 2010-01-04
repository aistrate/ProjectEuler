primeFactors n = primeFactors' n [] primeNumbers

primeFactors' 1 factors _ = factors
primeFactors' n factors (p:_) 
        | p * p > n       = factors ++ [n]
primeFactors' n factors primes@(p:ps)
        | n `mod` p == 0  = primeFactors' (n `div` p) (factors ++ [p]) primes
        | otherwise       = primeFactors' n factors ps


primeNumbers = primeNumbers' 2 []

primeNumbers' n primes =
        if any (n `dividedBy`) (takeWhile (`lessThanSqrtOf` n) primes)
        then primeNumbers' (n + 1) primes
        else n : primeNumbers' (n + 1) (primes ++ [n])
    where a `dividedBy` b       = a `mod` b == 0
          a `lessThanSqrtOf` b  = a * a <= b


result = maximum (primeFactors 600851475143)

main = print $ primeFactors 600851475147
