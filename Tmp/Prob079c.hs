import System.IO
import Data.Char (digitToInt, intToDigit)
import Data.List


main = do h <- openFile "keylog.txt" ReadMode
          c <- hGetContents h
          let triplets = map (map digitToInt) $ lines c
          let passcodes = shortestPasscodes triplets
          mapM_ putStrLn $ map (map intToDigit) passcodes
          hClose h
-- 73162890


addTriplet :: [Int] -> [Int] -> [[Int]]
addTriplet []     ps = [ps]
addTriplet (d:ds) ps = let (ltD, geD) = break (== d) ps
                       in if null geD
                            then insertions d ps
                            else map ((ltD ++ [d]) ++) $ addTriplet ds (tail geD)
    where insertions :: Int -> [Int] -> [[Int]]
          insertions d ls = concatMap (\(before, after) -> 
                                            map ((before ++ [d]) ++) (addTriplet ds after))
                              $ map (flip splitAt ls) [0..length ls]


shortestLists :: [[a]] -> [[a]]
shortestLists lss = let minLen = minimum $ map length lss
                    in filter (\ls -> length ls == minLen) lss


shortestPasscodes :: [[Int]] -> [[Int]]
shortestPasscodes []     = []
shortestPasscodes (t:ts) = 
    foldl (\ps t' -> shortestLists $ concatMap (shortestLists . addTriplet t') ps) [t] ts
