import System.IO

main :: IO ()
main = do 
       inh <- openFile "triangle.txt" ReadMode
       lines <- readAllLines inh
       let result = process lines
       print result
       hClose inh
-- 7273

readAllLines :: Handle -> IO [String]
readAllLines inh = 
    do ineof <- hIsEOF inh
       if ineof
          then return []
          else do inpStr <- hGetLine inh
                  rest <- readAllLines inh
                  return (inpStr : rest)


process lines = maximumTotal $ triangleList lines

triangleList lines = map (map (fromInteger . read) . words) lines


maximumTotal triangle = partialMaximum triangle zeros
    where zeros = take (1 + length triangle) $ repeat 0
          partialMaximum [] [maxTotal] = maxTotal
          partialMaximum trg maximums  = 
                partialMaximum (init trg) (twoRowMax (last trg) maximums)

twoRowMax fstRow sndRow = zipWith newMax fstRow (pairList sndRow)
    where pairList row = zipWith lst row (tail row)
          lst a b = [a, b]
          newMax x ys = maximum $ map (x +) ys
