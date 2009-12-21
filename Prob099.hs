import System.IO
import qualified Data.ByteString.Char8 as C8
import Data.List
import Data.Maybe (fromJust)

main = do h <- openFile "base_exp.txt" ReadMode
          cs <- hGetContents h
          let ls = map (map (read :: String -> Integer) . split ',') $ lines cs
          let numbers = map (\(x:y:_) -> (x, y)) ls
          let maxPow@(b, e) = maximumPower numbers
          print maxPow
          print . (1+) . fromJust $ elemIndex maxPow numbers
          hClose h
-- (895447,504922)
-- 709


split :: Char -> String -> [String]
split c = map C8.unpack . C8.split c . C8.pack


maximumPower :: [(Integer, Integer)] -> (Integer, Integer)
maximumPower ps = let compPowers (_, pp) (_, qq) = compare pp qq
                  in fst . maximumBy compPowers . map (\p -> (p, compValue p)) $ ps


-- b^e = exp(log (b^e)) = exp(e * log b)

compValue :: (Integer, Integer) -> Double
compValue (b, e) = let b' = fromIntegral b
                       e' = fromIntegral e
                   in e' * log b'
