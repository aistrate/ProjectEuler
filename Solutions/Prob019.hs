module Prob019 where

daysInMonth = [31, 28, 31, 30, 31, 30, 
               31, 31, 30, 31, 30, 31]

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Eq, Show)

data Date = Date { day   :: Int,
                   month :: Int,
                   year  :: Int }
    deriving (Eq, Show)


leapYear n | n `mod` 400 == 0 = True
           | n `mod` 100 == 0 = False
           | n `mod` 4   == 0 = True
           | otherwise        = False


-- 1 Jan 1900 is day zero
dateToInt :: Date -> Int
dateToInt d = (day d - 1) + monthToDays (month d) (year d) +
                            yearToDays (year d)
  where monthToDays m y = (sum $ take (m - 1) daysInMonth) +
                          if m > 2 && leapYear y then 1 else 0
        yearToDays y
            | y >= 1900 = sum . map daysInYear $ [1900..year d - 1]
            | otherwise = (-1) * (sum . map daysInYear $ [year d..1899])
        daysInYear y | leapYear y = 366
                     | otherwise  = 365


dateToDayOfWeek d = case dateToInt d `mod` 7 of
                        0 -> Mon
                        1 -> Tue
                        2 -> Wed
                        3 -> Thu
                        4 -> Fri
                        5 -> Sat
                        6 -> Sun

main = print $
       length . filter (== Sun) . map dateToDayOfWeek $
       [ Date 1 m y | y <- [1901..2000], m <- [1..12] ]
-- 171
