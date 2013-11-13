import Data.Array
import Data.List

maxSquare = 1000
squaresList = map (^ 2) [1..maxSquare]

squares = listArray (1, maxSquare) squaresList

areSquares = (listArray (1, maxSquare ^ 2) (cycle [False])) //
             (map (\s -> (s, True)) squaresList)


solutions n = [ (a, b, c) | a <- [3..(n `div` 3)],
                            b <- [a+1..(n `div` 2)],
                            let c2 = (squares ! a) + (squares ! b),
                            areSquares ! c2,
                            let c = truncate . sqrt $ fromIntegral c2,
                            a + b + c <= n ]


splitByPerimeter sols = reverse $ sortBy compareLength perimGroups
    where perimGroups = groupBy eqPerim . sortBy comparePerim
                            $ map calcPerim sols
          calcPerim (a, b, c) = (a + b + c, a, b, c)
          comparePerim (p, _, _, _) (q, _, _, _) = compare p q
          eqPerim a b = comparePerim a b == EQ
          compareLength g h = compare (length g) (length h)


main = print $
       head . splitByPerimeter $ solutions 1000
-- [(840,40,399,401),(840,56,390,394),(840,105,360,375),(840,120,350,370),
--  (840,140,336,364),(840,168,315,357),(840,210,280,350),(840,240,252,348)]

-- 840
