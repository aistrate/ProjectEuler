import Data.List (sort)


fibonacciNums = 1 : 1 : zipWith (+) fibonacciNums (tail fibonacciNums)

lastNineDigits :: Integer -> String
--lastNineDigits n = show $ n `mod` 1000000000
lastNineDigits = reverse . take 9 . reverse . show

firstNineDigits :: Integer -> String
firstNineDigits = take 9 . show

isPandigital :: String -> Bool
isPandigital = ("123456789" ==) . sort


result = head . filter (isDoublePandigital . snd) $ zip [1..] fibonacciNums
         where isDoublePandigital n = (isPandigital $ firstNineDigits n) &&
                                      (isPandigital $ lastNineDigits n)

--main = print result

main = print . fst . head . filter (isDoublePandigital . snd) $ zip [1..] fibonacciNums
       where isDoublePandigital n = let s = show n
                                        firstNine = take 9 s
                                        lastNine = reverse . take 9 . reverse $ s
                                    in isPandigital firstNine && isPandigital lastNine
