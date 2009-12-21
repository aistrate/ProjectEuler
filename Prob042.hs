import System.IO
import Data.Char (toUpper)
import Data.Array
import Data.List

letters = listArray ('A', 'Z') [1..26]

isTriangleWord w = isTriangleNumber . sum $ map ((letters !) . toUpper) w


triangleNumbers = [ n * (n + 1) `div` 2 | n <- [1..] ]

isTriangleNumber n = n == head (dropWhile (< n) triangleNumbers)


main :: IO ()
main = do 
       inh <- openFile "words.txt" ReadMode
       inpStr <- hGetContents inh
       let result = process inpStr
       print result
       hClose inh


process inp = length $ filter isTriangleWord words
    where words = read $ "[" ++ inp ++ "]"
-- 162
