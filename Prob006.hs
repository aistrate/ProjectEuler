diffSquares n = (sum [1..n])^2 - sum [ x * x | x <- [1..n] ]

main = print $
       diffSquares 100
-- 25164150
