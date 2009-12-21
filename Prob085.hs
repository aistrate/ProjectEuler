import Data.List (sortBy)


subrectSizes (rows, cols) = [ (row, col) | row <- [1..rows], col <- [1..cols] ]

countSubrects (rows, cols) (subRows, subCols) = (rows - subRows + 1) * (cols - subCols + 1)

countAllSubrects (rows, cols) = sum . map (countSubrects (rows, cols)) $ subrectSizes (rows, cols)


countsLessThan n = concat $ takeWhile (not . null) [ rowLessThan n row | row <- [1..] ]
    where rowLessThan n row = takeWhile ((< n) . snd) [ ((row, col), countAllSubrects (row, col)) | col <- [1..row] ]


interval :: Int -> (Int, Int)
interval n = let d = fromIntegral n
             in (round $ d * 0.98, round $ d * 1.02)


closestAreasTo a = let (loLimit, hiLimit) = interval a
                       candidates = filter ((> loLimit) . snd) $ countsLessThan hiLimit
                       sorted = sortBy (\p q -> compare (dist p) (dist q)) candidates
                       dist (_, d) = abs (d - a)
                       shortestDist = snd $ head sorted
                   in takeWhile ((== shortestDist) . snd) sorted


main = do let areas = closestAreasTo 2000000
          let ((rows, cols), _) = head areas
          print areas
          print $ rows * cols
-- Compiled (< 2 sec.):
-- [((77,36),1999998)]
-- 2772
