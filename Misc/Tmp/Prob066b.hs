import Data.List ((\\), maximumBy)
import qualified Data.Map as Map

maxN = 1000000
maxN2 = maxN * maxN
mn = 200000000^2

squares = [ n * n | n <- [1..] ]

squaresMap = Map.fromList [ (n * n, n) | n <- [1..maxN] ]

isSquare n | n <= maxN2 = (Map.lookup n squaresMap) /= Nothing
           | n <= mn    = let r = sqrt $ fromIntegral n
                          in r == (fromIntegral $ truncate r)
           | otherwise  = True


solveFor d = firstSqrt $ map ySide squares
    where ySide y2 = 1 + d * y2
          firstSqrt = sqRoot . head . filter isSquare
          sqRoot = truncate . sqrt . fromIntegral


dValues n = [1..n] \\ (takeWhile (<= n) squares)

solutions = map sol . dValues
    where sol d = (d, solveFor d)

maxD n = fst . head $ dropWhile ((/= maxX) . snd) sols
    where sols = solutions n 
          maxX = snd $ maximumBy compSol sols
          compSol p q = compare (snd p) (snd q)


result = maxD 1000


isSq n = n `elem` takeWhile (<= n) squares



















{-
[(2,3),(3,2),(5,9),(6,5),(7,8),(8,3),(10,19),(11,10),(12,7),(13,649),
(14,15),(15,4),(17,33),(18,17),(19,170),(20,9),(21,55),(22,197),
(23,24),(24,5),(26,51),(27,26),(28,127),(29,9801),(30,11),(31,1520),
(32,17),(33,23),(34,35),(35,6),(37,73),(38,37),(39,25),(40,19),
(41,2049),(42,13),(43,3482),(44,199),(45,161),(46,24335),(47,48),
(48,7),(50,99),(51,50),(52,649),(53,66249),(54,485),(55,89),(56,15),
(57,151),(58,19603),(59,530),(60,31),(61,10000001),(62,63),(63,8),
(65,129),(66,65),(67,48842),(68,33),(69,7775),(70,251),(71,3480),
(72,17),(73,2281249),(74,3699),(75,26),(76,57799),(77,351),(78,53),
(79,80),(80,9),(82,163),(83,82),(84,55),(85,285769),(86,10405),
(87,28),(88,197),(89,500001),(90,19),(91,1574),(92,1151),(93,12151),
(94,2143295),(95,39),(96,49),(97,10000008),(98,99),(99,10)]

(61,10000001)
(73,2281249)        x=2281249, d=73, y=267000
(94,2143295)        x=2143295, d=94, y=221064
(97,10000008)
-}
