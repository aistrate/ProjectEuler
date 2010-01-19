import Timer
import Prob003


squares = map (^2) [2..]

candidates :: [Integer] -> [Integer] -> [[Integer]]
candidates xs@(x:xs') ys@(y:ys') | x < y     = candidates (dropWhile (< y) xs') ys
                                 | x > y     = candidates xs (dropWhile (< x) ys')
                                 | otherwise = (take 6 xs) : candidates xs' ys'

hasProperty :: [Integer] -> Bool
hasProperty xs = let n2 = head xs - 1
                 in xs == map ($ n2) [(+1), (+3), (+7), (+9), (+13), (+27)]


main = printTime . print . filter ((<= (10^12 + 1)) . head) .
       filter hasProperty $ candidates primeNumbers (map (+1) squares)
