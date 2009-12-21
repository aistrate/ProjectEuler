import Char (digitToInt)
 

digits = map digitToInt . show

next = sum . map (^2) . digits

chain = iterate next

chainEnd = head . dropWhile (\n -> n /= 1 && n /= 89) . chain


main = print . length . filter (== 89) $ map chainEnd [1..10000000]
-- 8581146 (compiled: 52 sec.)
