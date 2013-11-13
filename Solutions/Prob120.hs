import Timer


remainder a n = ((a - 1)^n + (a + 1)^n) `mod` (a^2)

extractPeriod ns = let period = head . filter isPeriod
                                $ candidatePeriods (head ns) (tail ns)
                   in take period ns
  where candidatePeriods x ys = map fst . filter ((x ==) . snd) $ zip [1..] ys
        isPeriod p = let (fs, rs) = splitAt p ns
                     in and . take 10 $ zipWith (==) fs rs

rMax a = maximum . extractPeriod $ map (remainder a) [1..]


main = let rs = map (\n -> (n, rMax n)) $ [3..1000]
       in printTime $
          print $
          sum $ map snd rs
-- 333082500
-- Time: 57.953125 sec.
-- (compiled)
