module Prob038 (digitList, isPandigital) where

import Data.Char (digitToInt)
import Data.List

digitList n = map (fromIntegral . digitToInt) $ show n


isPandigital :: Integer -> Bool
isPandigital n = not (0 `elem` digits) && length digits == 9 &&
                 length (nub digits) == 9
                 where digits = digitList n


concatenatedProduct :: Integer -> [Integer] -> Integer
concatenatedProduct n ds = read . concat $ map (show . (n*)) ds


first9DigitProd n = head $ dropWhile (< 100000000) prods
    where prods = map (concatenatedProduct n) (tail . tail $ inits [1..])

pandigitals = [ pan | pan <- [9..9999], isPandigital (first9DigitProd pan) ]


main = print $
       last . sort $ map first9DigitProd pandigitals
-- 932718654

-- map (\p -> (p, first9DigitProd p)) pandigitals
