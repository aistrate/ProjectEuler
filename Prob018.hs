import GHC.Arr

triangleList = [
    [75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10],
    [20, 04, 82, 47, 65],
    [19, 01, 23, 75, 03, 34],
    [88, 02, 77, 73, 07, 63, 67],
    [99, 65, 04, 28, 06, 16, 70, 92],
    [41, 41, 26, 56, 83, 40, 80, 70, 33],
    [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23] ]

triangleArray = listArray ((1, 1), (len, len)) $ concat tidyList
   where len = length $ last triangleList
         compl xs = take len $ xs ++ replicate len 0
         tidyList = map compl triangleList


nextIndices arr = array (bounds arr) newAssocs
    where newAssocs   = map (\ix -> (ix, next ix)) (indices arr)
          next (i, j) = [(i+1, j), (i+1, j+1)]


allPaths levels start nextIxs = allPaths' levels start
    where allPaths' 1 ix = [[ix]]
          allPaths' n ix = concat $ map (map (ix:) . allPaths' (n-1)) 
                                        (nextIxs ! ix)


result = maximum $ map value paths
         where paths = allPaths 15 (1, 1) (nextIndices triangleArray)
               value path = sum $ map (triangleArray !) path

-- 1074

-- start from the bottom of the triangle!
