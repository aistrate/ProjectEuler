letters :: Int -> String

letters 1 = "one"
letters 2 = "two"
letters 3 = "three"
letters 4 = "four"
letters 5 = "five"
letters 6 = "six"
letters 7 = "seven"
letters 8 = "eight"
letters 9 = "nine"

letters 10 = "ten"
letters 11 = "eleven"
letters 12 = "twelve"
letters 13 = "thirteen"
letters 14 = "fourteen"
letters 15 = "fifteen"
letters 16 = "sixteen"
letters 17 = "seventeen"
letters 18 = "eighteen"
letters 19 = "nineteen"

letters 20 = "twenty"
letters 30 = "thirty"
letters 40 = "forty"
letters 50 = "fifty"
letters 60 = "sixty"
letters 70 = "seventy"
letters 80 = "eighty"
letters 90 = "ninety"

letters n | n > 20 && n < 100 && n `mod` 10 /= 0 =
    letters (10 * (n `div` 10))  ++ letters (n `mod` 10)

letters n | n >= 100 && n < 1000 && n `mod` 100 == 0 =
    letters (n `div` 100) ++ "hundred"

letters n | n > 100 && n < 1000 && n `mod` 100 /= 0 =
    letters h ++ "and" ++ letters (n - h)
    where h = (100 * (n `div` 100))

letters 1000 = "onethousand"

result = length . concat $ map letters [1..1000]
-- 21124
