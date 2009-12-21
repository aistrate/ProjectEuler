import Prob038 (digitList)

digitalSum :: Integer -> Int
digitalSum = sum . digitList

numbers = [ a ^ b | a <- [1..100], b <- [1..100] ]


result = maximum $ map digitalSum numbers
-- 972


maxPairs = [ (a, b) | a <- [1..100], b <- [1..100], 
                      digitalSum (a ^ b) == 972 ]
-- [(99,95)]
