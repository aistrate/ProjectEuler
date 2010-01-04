import Data.Char (digitToInt)

sumFifth n = sum . map ((^5) . digitToInt) $ show n

fifthNumbers n = filter isFifth [2..n]
                 where isFifth k = k == sumFifth k

result = sum $ fifthNumbers 300000
-- 443839
