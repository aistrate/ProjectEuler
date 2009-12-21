fibonacci :: (Int -> Bool) -> Int -> Int -> Int -> Int
fibonacci p a b s | p b       = fibonacci p b (a + b) (addIfEven b s)
                  | otherwise = s
    where addIfEven n sm = if n `mod` 2 == 0
                           then n + sm
                           else sm

result = fibonacci (< 4000000) 1 1 0



-- 4613732
