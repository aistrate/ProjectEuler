import Data.List (intersect)
import qualified Data.Map as Map

dValues = [ d | d <- [1..1000], not (d `elem` squares) ]
    where squares = takeWhile (<= 1000) $ map (^2) [1..]

solveFor :: Integer -> Integer
solveFor d = head . (++ [0]) . concat $ map trySolution [2..2000000]
    where trySolution x = if (x*x - 1) `mod` d == 0
                          then let y = truncate . sqrt . fromIntegral
                                            $ (x*x - 1) `div` d
                               in if x*x - d * y*y == 1
                                  then [x]
                                  else []
                          else []

squares = Map.fromList [ (n * n, True) | n <- [1..2000000] ]
isSquare n = (Map.lookup n squares) /= Nothing


maxN = 2000000
xValues = [ x*x - 1 | x <- [2..maxN] ]
yValues = [ y*y | y <- [2..maxN] ]

solveFor2 d = head $
    intersect (map (`div` d) $ filter (\x -> x `mod` d == 0) xValues)
              yValues


result = map solveFor dValues

main = print $ maximum result
-- Compiled (max 2000000, >3min): 1907162
