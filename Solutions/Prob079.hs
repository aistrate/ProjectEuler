import System.IO
import Data.List


main = do h <- openFile "keylog.txt" ReadMode
          c <- hGetContents h
          let triplets = lines c
          let passcodes = shortestPasscodes triplets
          mapM_ putStrLn passcodes
          hClose h
-- 73162890


addTriplet :: String -> String -> [String]
addTriplet []     ps = [ps]
addTriplet (d:ds) ps = let (ltD, geD) = break (== d) ps
                       in if null geD
                            then insertions d ps
                            else map ((ltD ++ [d]) ++) $ addTriplet ds (tail geD)
    where insertions :: Char -> String -> [String]
          insertions d ls = concatMap (\(before, after) -> 
                                            map ((before ++ [d]) ++) (addTriplet ds after))
                              $ map (flip splitAt ls) [0..length ls]


shortest :: [[a]] -> [[a]]
shortest lss = let minLen = minimum $ map length lss
               in filter (\ls -> length ls == minLen) lss


shortestPasscodes :: [String] -> [String]
shortestPasscodes []     = []
shortestPasscodes (t:ts) = foldl (\ps t' -> shortest $ concatMap (shortest . addTriplet t') ps) [t] ts
