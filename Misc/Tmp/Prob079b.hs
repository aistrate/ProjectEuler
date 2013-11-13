import System.IO
import Data.Char (digitToInt, intToDigit)
import Data.List


main = do h <- openFile "keylog.txt" ReadMode
          c <- hGetContents h
          let triplets = map (map digitToInt) $ lines c
          mapM_ print $ map (map (map intToDigit) . shortestPasscodes) (inits triplets)
          --let passcodes = shortestPasscodes triplets
          --mapM_ putStrLn $ map (map intToDigit) passcodes
          hClose h
-- 73162890


data Passcode = PasscodeList [Int] |
                ExistingDigit [Int] Int Passcode |
                InsertedDigit [Int] Int Passcode
    deriving (Eq, Show)


addTriplet :: [Int] -> [Int] -> [Passcode]
addTriplet []     ps = [PasscodeList ps]
addTriplet (d:ds) ps = let (ltD, geD) = break (== d) ps
                           choices = if null geD
                                        then insertions d ps
                                        else [ExistingDigit ltD d (PasscodeList $ tail geD)]
                       in concatMap (mapOnChild (\(PasscodeList ls) -> addTriplet ds ls)) choices


mapOnChild :: (Passcode -> [Passcode]) -> Passcode -> [Passcode]
mapOnChild f (ExistingDigit ls d p) = map (\p' -> ExistingDigit ls d p') (f p)
mapOnChild f (InsertedDigit ls d p) = map (\p' -> InsertedDigit ls d p') (f p)


insertions :: Int -> [Int] -> [Passcode]
insertions d ls = map (\(before, after) -> InsertedDigit before d (PasscodeList after))
                    $ map (flip splitAt ls) [0..length ls]


flatten :: Passcode -> [Int]
flatten (PasscodeList ls) = ls
flatten (ExistingDigit before d p) = before ++ (d : flatten p)
flatten (InsertedDigit before d p) = before ++ (d : flatten p)


shortestLists :: [[a]] -> [[a]]
shortestLists lss = let minLen = minimum $ map length lss
                    in filter (\ls -> length ls == minLen) lss


addShortest :: [Int] -> [Int] -> [[Int]]
addShortest t = shortestLists . map flatten . addTriplet t


shortestPasscodes :: [[Int]] -> [[Int]]
shortestPasscodes []     = []
shortestPasscodes (t:ts) = foldl (\ps t' -> shortestLists $ concatMap (addShortest t') ps) [t] ts
