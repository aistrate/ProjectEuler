import Data.List ((\\))

truncRoot = truncate . sqrt . fromIntegral

rewriteFrac m n p = (a, n, b, c)
    where c = quot (n - p*p) m
          largestB = truncRoot n
          candidateA candB = quotRem (candB + p) c
          candidatesA = map candidateA $ reverse [1..largestB]
          a = (fst . head) $ filter ((== 0) . snd) candidatesA
          b = a * c - p

continuedFrac x = map fstOfQuad $ expansion 1 x first []
    where fstOfQuad (f, _, _, _) = f
          first = truncRoot x
          expansion m n p ts 
                    | elem next ts = ts
                    | otherwise    = expansion c n b (ts ++ [next])
                where next = rewriteFrac m n p
                      (_, _, b, c) = next

periodContFrac = length . continuedFrac

nonSquares n = [1..n] \\ (map (^2) [1..(truncRoot n)])

result = length . filter odd . map periodContFrac $ nonSquares 10000
