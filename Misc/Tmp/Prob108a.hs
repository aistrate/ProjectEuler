-- 1/x + 1/y = 1/n
-- n(x + y) = xy
-- y(x - n) = nx
-- y = nx / (x - n)

import Timer


solutions :: Integer -> [(Integer, Integer)]
solutions n = [ (x, y) | x <- [n+1..2*n], (n * x) `mod` (x - n) == 0, 
                                          let y = (n * x) `div` (x - n) ]

result = head . dropWhile ((<= 1000) . snd) 
         $ map (\n -> (n, length (solutions n))) [12,24..]

main = printTime $ print result
-- (180180,1013)
-- Time: 532.384661 sec.
-- (8min 52s, compiled)
