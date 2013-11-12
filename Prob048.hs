series n = map (\n -> n ^ n) [1..n]

main = print $
       ((read . reverse . take 10 . reverse . show . sum $ series 1000)::Integer)
-- 9110846700
