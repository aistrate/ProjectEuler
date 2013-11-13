import Data.List (find)
import System.IO


n2r = [(1000, "M"), (900, "CM"),
       (500, "D"), (400, "CD"),
       (100, "C"), (90, "XC"),
       (50, "L"), (40, "XL"),
       (10, "X"), (9, "IX"),
       (5, "V"), (4, "IV"),
       (1, "I")]

r2n = map (\(f, s) -> (s, f)) n2r


number2roman :: Int -> String
number2roman n = snd $ foldl extract (n, "") n2r
    where extract (k, rs) (denom, romNum) = (k `mod` denom, 
                                             rs ++ (concat $ replicate (k `div` denom) romNum))


roman2number :: String -> Int
roman2number rs = fst . head 
                . dropWhile (\(_, rs'') -> length rs'' > 0) 
                $ iterate addMatch (0, rs)
    where addMatch (n, rs') = case find (\(r, k) -> r == take (length r) rs') r2n of
                                 Just (r, k) -> (n + k, drop (length r) rs')
                                 Nothing     -> error "Not a roman numeral"


main = do h <- openFile "roman.txt" ReadMode
          c <- hGetContents h
          let romanNums = lines c
          print . sum $ map (\r -> length r - length (number2roman $ roman2number r)) romanNums
          hClose h
    where pair rs = (rs, number2roman $ roman2number rs)
-- 743
