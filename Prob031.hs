coins = [200, 100, 50, 20, 10, 5, 2, 1]

coverage amt [c] 
    | amt `mod` c == 0 = [[(amt `div` c, c)]]
    | otherwise        = []
coverage amt (c:cs) = concat [ map ((nc, c) :) covRest |
                               nc <- (reverse [0..(amt `div` c)]),
                               let covRest = coverage (amt - nc * c) cs ]

printCoverage amt cs = do
    mapM_ print $ coverage amt cs
    print (length cov)
    where cov = coverage amt cs


result = length $ coverage 200 coins
-- 73682
