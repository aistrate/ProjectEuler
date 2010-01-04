import Data.List (sort)


type Point = (Int, Int)

points :: Int -> [Point]
points n = tail [ (x, y) | x <- [0..n], y <- [0..n] ]

triangles :: Int -> [(Point, Point)]
triangles n = let pts = points n
              in [ (p, q) | p <- pts, q <- tail $ dropWhile (/= p) pts ]

orig :: Point
orig = (0, 0)

isRightTriangle :: (Point, Point) -> Bool 
isRightTriangle (p, q) = let (a2:b2:c2:[]) = sort [dist2 p orig, dist2 q orig, dist2 p q]
                         in a2 + b2 == c2
    where dist2 (px, py) (qx, qy) = (px - qx)^2 + (py - qy)^2


main = print . length . filter isRightTriangle $ triangles 50
-- 14234 (compiled: 4 sec.)
