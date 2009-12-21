import Data.List
import qualified Data.Map as Map

triangle   = [ n * (n + 1) `div` 2      | n <- [1..] ]
square     = [ n * n                    | n <- [1..] ]
pentagonal = [ n * (3 * n - 1) `div` 2  | n <- [1..] ]
hexagonal  = [ n * (2 * n - 1)          | n <- [1..] ]
heptagonal = [ n * (5 * n - 3) `div` 2  | n <- [1..] ]
octagonal  = [ n * (3 * n - 2)          | n <- [1..] ]

filter4Digits = (takeWhile (\n -> n <= 9999)) . (dropWhile (\n -> n < 1000))

numbers = [3..8]
numberLists = map filter4Digits [triangle, square, pentagonal, 
                                 hexagonal, heptagonal, octagonal]

numberListLinks = Map.fromList 
      [ ((fst ps, fst rs), links (snd ps) (snd rs)) | ps <- assc, 
                                                      rs <- assc, ps /= rs ]
    where assc = zip numbers numberLists


links xs ys = [ [x, y] | x <- xs, y <- ys, x `mod` 100 == y `div` 100 ]

chains (ps:[])   = ps
chains (ps:rest) = [ (head p) : r | p <- ps, r <- rs, last p == head r ]
                   where rs = chains rest


cycles ns = map init . filter isCyclic $ chains linkLists
    where linkLists = map (numberListLinks Map.!) 
                            $ zip ns (tail ns ++ [head ns])
          isCyclic x = head x == last x


result0 = map head . group $ map sort allCycles
    where allCycles = concat . map cycles $ permutations numbers
-- [[1281,2512,2882,5625,8128,8256]]


result = sum . head . concat . map cycles $ permutations numbers
-- [8128,2882,8256,5625,2512,1281]
-- 28684
