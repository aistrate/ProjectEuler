module Prob036 (isPalindrome) where

binary 0 = []
binary n = (binary (n `div` 2)) ++ [n `mod` 2]

isPalindrome xs = take hlen xs == take hlen (reverse xs)
    where hlen = length xs `div` 2

isDoublePalindrome n = isPalindrome (show n) && isPalindrome (binary n)

doublePalindromes n = filter isDoublePalindrome [1..n]
-- [1,3,5,7,9,33,99,313,585,717,7447,9009,15351,32223,39993,53235,
--  53835,73737,585585]


main = print $
       sum $ doublePalindromes 1000000
-- 872187
