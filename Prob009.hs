triplets maxa maxb s = [ (a, b, c) | a <- [3..maxa], b <- [a + 1 .. maxb],
                                     let c = s - a - b,
                                     a * a + b * b == c * c ]

main = print $
       let (a, b, c) = head $ triplets 500 500 1000
       in a * b * c
-- 31875000

-- [(200,375,425)]
