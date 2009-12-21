import System.IO
import qualified Data.ByteString.Char8 as C8
import Data.List (unfoldr, sort)


main = do h <- openFile "triangles.txt" ReadMode
          cs <- hGetContents h
          let ls = map (map (read :: String -> Int) . split ',') $ lines cs
          let triangles = map (map (\(x:y:_) -> (x, y)) . splitEvery 2) ls
          print . length . filter containsOrigin $ triangles
          hClose h
-- 228


split :: Char -> String -> [String]
split c = map C8.unpack . C8.split c . C8.pack

splitEvery :: Int -> [a] -> [[a]]
splitEvery k = unfoldr (\ns -> if null ns then Nothing else Just (take k ns, drop k ns))


containsOrigin :: [(Int, Int)] -> Bool
containsOrigin ps = all (< pi) . interAngles $ ps


-- polar coordinates angle, 0 to 2*pi
theta :: (Int, Int) -> Double
theta (x, y) = let absDouble = fromIntegral . abs
                   posTheta = atan (absDouble y / absDouble x)
               in case (compare x 0, compare y 0) of
                    (GT, GT) -> posTheta
                    (GT, EQ) -> 0
                    (GT, LT) -> 2 * pi - posTheta
                    (EQ, GT) -> pi / 2
                    (EQ, EQ) -> 0
                    (EQ, LT) -> 3 * pi / 2
                    (LT, GT) -> pi - posTheta
                    (LT, EQ) -> pi
                    (LT, LT) -> pi + posTheta


-- by definition, the sum of all inter-angles is 2*pi
interAngles :: [(Int, Int)] -> [Double]
interAngles ps = let thetas = sort $ map theta ps
                 in zipWith (-) (tail thetas ++ [head thetas + 2 * pi]) thetas
