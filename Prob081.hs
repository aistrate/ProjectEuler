import System.IO
import Data.Array
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as C8


main = do h <- openFile "matrix.txt" ReadMode
          c <- hGetContents h
          print . minimalPath $ readMatrix c
          hClose h
-- 427337    (max: 1185980)


readMatrix :: String -> Array (Int, Int) Int
readMatrix cs = let ls = map (map (read . C8.unpack) . C8.split ',' . C8.pack) $ lines cs
                    rows = length ls
                    cols = length $ head ls
                in listArray ((1, 1), (rows, cols)) $ concat ls


minimalPath :: Array (Int, Int) Int -> Int
minimalPath m = snd . head $ foldr pathsFrom lastDiagPaths (init diags)
    where ((_, _), (size, _)) = bounds m
          diags = diagonals size
          lastDiagPaths = map (\ix -> (ix, m ! ix)) $ last diags
          pathsFrom :: [(Int, Int)] -> [((Int, Int), Int)] -> [((Int, Int), Int)]
          pathsFrom diag nextDiagPaths = let minNextFrom ix = minimum $ map (fromJust . flip lookup nextDiagPaths) 
                                                                            (nextMoves size ix)
                                         in map (\ix -> (ix, (m ! ix) + minNextFrom ix)) diag


diagonals :: Int -> [[(Int, Int)]]
diagonals size = map upperDiagonal [1..size] ++
                 map lowerDiagonal [2..size]
    where upperDiagonal row = map (\col -> (row - col + 1, col)) [1..row]
          lowerDiagonal col = map (\row -> (row, col + size - row)) [size,size-1..col]


nextMoves :: Int -> (Int, Int) -> [(Int, Int)]
nextMoves size (row, col) = (if row < size then [(row + 1, col)] else []) ++
                            (if col < size then [(row, col + 1)] else [])
