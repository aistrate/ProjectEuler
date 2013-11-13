import System.IO
import qualified Data.ByteString.Char8 as C8
import Data.List
import Data.Maybe (fromJust)

main = do h <- openFile "base_exp.txt" ReadMode
          cs <- hGetContents h
          let ls = map (map (read :: String -> Integer) . split ',') $ lines cs
          let numbers = map (\(x:y:_) -> (x, y)) ls
          let maxPow@(b, e) = maximumPower 10 $ take 20 numbers
          print maxPow
          print . (1+) . fromJust $ elemIndex maxPow numbers
          print . length $ show (b^e)
          hClose h
-- Compiled (about 12 min.):
-- (895447,504922)
-- 709
-- 3005316


split :: Char -> String -> [String]
split c = map C8.unpack . C8.split c . C8.pack

splitEvery :: Int -> [a] -> [[a]]
splitEvery k = unfoldr (\ns -> if null ns then Nothing else Just (take k ns, drop k ns))


maximumPower :: Int -> [(Integer, Integer)] -> (Integer, Integer)
maximumPower k ps 
    | length ps <= k = let compPowers (_, pp) (_, qq) = compare pp qq
                       in fst . maximumBy compPowers . map (\p@(b, e) -> (p, b^e)) $ ps
    | otherwise      = maximumPower k . map (maximumPower k) . splitEvery k $ ps


{-maximumPower ps = let compPowers (_, pp) (_, qq) = compare pp qq
                      powerLens = map (\p@(b, e) -> (p, length $ show (b^e))) ps
                      maxLenPower = snd . maximumBy compPowers $ powerLens
                      maxLenPowers = map fst . filter ((== maxLenPower) . snd) $ powerLens
                  in fst . maximumBy compPowers . map (\p@(b, e) -> (p, b^e)) $ maxLenPowers-}

{-maximumPower :: [(Integer, Integer)] -> (Integer, Integer)
maximumPower ps = let compPowers (_, pp) (_, qq) = compare pp qq
                  in fst . maximumBy compPowers . map (\p@(b, e) -> (p, b^e)) $ ps-}

{-maximumPower :: [(Integer, Integer)] -> (Integer, Integer)
maximumPower ps = let compPowers (_, pp) (_, qq) = compare pp qq
                  in fst . maximumBy compPowers . map (\p@(b, e) -> (p, (b^e) `div` 2097152)) $ ps-}
