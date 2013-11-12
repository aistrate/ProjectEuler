module Prob064 where

import Data.List ((\\))

truncRoot :: Int -> Int
truncRoot = truncate . sqrt . fromIntegral


-- Every fraction of the form:
--      m / ( sqrt(n) - p )
-- can be uniquely rewritten as:
--      a + ( sqrt(n) - b ) / c
-- so that b is the maximum integer for which: sqrt(n) - b > 0;
-- Signature: rewriteFrac m n p ==> (a, n, b, c)
-- In the caller, a will be added to the sequence,
-- then 'rewriteFrac c n b' will be called.
rewriteFrac :: Int -> Int -> Int -> (Int, Int, Int, Int)
rewriteFrac m n p = (a, n, b, c)
    where c = quot (n - p*p) m
          largestB = truncRoot n
          candidateA candB = quotRem (candB + p) c
          candidatesA = map candidateA $ reverse [1..largestB]
          a = (fst . head) $ filter ((== 0) . snd) candidatesA
          b = a * c - p


continuedFrac :: Int -> (Int, [Int])
continuedFrac x = (first, map fstOfQuad $ expansion 1 x first [])
    where fstOfQuad (f, _, _, _) = f
          first = truncRoot x
          expansion m n p ts 
                    | elem next ts = ts
                    | otherwise    = expansion c n b (ts ++ [next])
                where next = rewriteFrac m n p
                      (_, _, b, c) = next


periodContFrac = length . snd . continuedFrac


nonSquares n = [1..n] \\ (map (^2) [1..(truncRoot n)])


main = print $
       length . filter odd . map periodContFrac $ nonSquares 10000
-- 1322


{-
cont :: Int -> Int -> Int -> [Int]
cont r n d = m : rest
 where
 m = truncate ((sqrt (fromIntegral r) + fromIntegral n ) / fromIntegral d)
 a = n - d * m
 rest = if d == 1 && n /= 0
         then []
         else cont r (-a) ((r - a ^ 2) `div` d)
-}
