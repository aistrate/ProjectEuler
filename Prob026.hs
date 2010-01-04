import Data.List (tails, sortBy, isPrefixOf)

reciprocal n = divide 10 n
    where divide 0 b = []
          divide a b = (a `div` b) : divide (10 * (a `mod` b)) b


s `isCycleIn` digits = not (null $ drop (length candidate) digits) &&
                       take (length candidate) digits == candidate
                       where candidate = concat $ replicate 3 s


listCycle ns = cycle' [] ns
    where cycle' passed [] = []
          cycle' passed digits@(d:ds) = 
                if null cycles then cycle' (passed ++ [d]) ds
                               else head cycles
                where cycles = filter (`isCycleIn` digits) 
                                      (init $ tails passed)

recCycle n = listCycle $ reciprocal n


topLengths n = reverse . sortBy comp . map pair $ [2..n]
    where pair n = (n, length . recCycle $ n)
          comp p q = compare (snd p) (snd q)


result = head $ topLengths 1000

main = print result
-- Compiled: (983,982)


printReciprocals = mapM_ print $ map (take 20 . reciprocal) [2..100]

printCycles = mapM_ print $ map triplet [2..100]
    where triplet n = (n, take 10 (reciprocal n), recCycle n)

printNonPrefixes = mapM_ print
    $ map (\(n, r) -> (n, take 20 r, recCycle n))
    $ filter (\(n, r) -> not (listCycle r `isPrefixOf` dropWhile (== 0) r)) 
    $ map (\n -> (n, reciprocal n)) [2..200]
