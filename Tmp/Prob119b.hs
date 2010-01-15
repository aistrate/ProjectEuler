import Control.Arrow (arr, first)
import Data.List (minimumBy)
import Data.Char (ord)
import Timer


sorted ext ls = let (minHs, ls') = extractMins ls
                in minHs : sorted ext ls'
  where extractMins ls = let minH = minimum $ map (ext . head) ls
                             newLs = map (\l -> if ext (head l) == minH 
                                                then ([head l], tail l) else ([], l)) ls
                         in first (arr concat) $ unzip newLs

powers :: [[(Integer, Integer, Integer)]]
powers = map (\e -> map (\b -> (b^e, b, e)) [2..]) [2..12]

fst3 (a, _, _) = a

sortedPowers :: [(Integer, Integer, Integer)]
sortedPowers = concat $ sorted fst3 powers


sumDigits :: Integer -> Integer
sumDigits n = sum . map (\c -> fromIntegral(ord c - ord '0')) $ show n

hasProperty (n, b, _) = sumDigits n == b

propertyNums = dropWhile ((< 10) . fst3) . filter hasProperty $ sortedPowers


main = printTime . mapM_ print . zip [1..30] $ propertyNums
-- ...
-- (30,(248155780267521,63,8))
-- Time: 137.515625 sec.
-- (Compiled)
