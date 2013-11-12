comb n r = product [r+1..n] `div` product [1..n-r]

combs = [ comb n r | n <- [23..100], r <- [1..n] ]


main = print $
       length $ filter (> 1000000) combs
-- 4075
