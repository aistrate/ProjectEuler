diffSquares n = (sum [1..n])^2 - sum [ x * x | x <- [1..n] ]

result = diffSquares 100
-- 25164150
