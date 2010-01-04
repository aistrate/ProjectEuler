import Data.Char (digitToInt)

result = sum . (map digitToInt) . show $ 2^1000
-- 1366
