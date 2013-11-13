import System.IO
import Data.Array
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as C8


main = do h <- openFile "matrix.txt" ReadMode
          c <- hGetContents h
          print . minimalPath $ readMatrix c
          hClose h
-- 260324


readMatrix :: String -> Array (Int, Int) Int
readMatrix cs = let ls = map (map (read . C8.unpack) . C8.split ',' . C8.pack) $ lines cs
                    rows = length ls
                    cols = length $ head ls
                in listArray ((1, 1), (rows, cols)) $ concat ls


minimalPath :: Array (Int, Int) Int -> Int
minimalPath m = minimum . map snd $ foldr pathsFrom lastColPaths (init cols)
    where ((_, _), (size, _)) = bounds m
          cols = columns size
          lastColPaths = map (\ix -> (ix, m ! ix)) $ last cols
          pathsFrom :: [(Int, Int)] -> [((Int, Int), Int)] -> [((Int, Int), Int)]
          pathsFrom column nextColPaths = 
                let costFrom (row1, col1) ix2@(row2, _) = (sum . map (m !) $ verticalMove col1 row1 row2)
                                                        + (fromJust $ lookup ix2 nextColPaths)
                in map (\ix1 -> (ix1, minimum $ map (costFrom ix1) (nextMoves size ix1))) column


columns :: Int -> [[(Int, Int)]]
columns size = map (\col -> map (\row -> (row, col)) [1..size]) [1..size]


nextMoves :: Int -> (Int, Int) -> [(Int, Int)]
nextMoves size (_, col) | col == size = []
                        | otherwise   = map (\row -> (row, col + 1)) [1..size]


verticalMove :: Int -> Int -> Int -> [(Int, Int)]
verticalMove col row1 row2 = let (row1', row2') = (min row1 row2, max row1 row2)
                             in map (\row -> (row, col)) [row1'..row2']
