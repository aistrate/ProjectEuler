import Data.List (sort)
import Timer


fibonacci = (1, 1) : (1, 1) : zipWith add fibonacci (tail fibonacci)
  where add (a, a') (b, b') = (a + b, (a' + b') `mod` 1000000000)

firstNineDigits (n, n') = take 9 $ show n
lastNineDigits (n, n') = show n'

isPandigital = (['1'..'9'] ==) . sort

isDoublePandigital :: (Integer, Integer) -> Bool
isDoublePandigital n = (isPandigital $ lastNineDigits n) &&
                       (isPandigital $ firstNineDigits n)


main = printTime $
       print $
       fst . head . filter (isDoublePandigital . snd) $
       zip [1..] fibonacci
-- 329468
-- Time: 4.656250 sec.
-- (compiled)
