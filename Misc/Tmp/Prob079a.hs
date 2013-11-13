import System.IO
import Data.Char (digitToInt)
import Data.List
import Data.Maybe


main = do h <- openFile "keylog.txt" ReadMode
          c <- hGetContents h
          let ls = lines c
          let triplets = map (map digitToInt) ls
          --let passcode = shortestPasscode . take 2 $ drop 9 triplets
          --let passcode = nub $ concat triplets
          --let passcode = shortestPasscode triplets
          --print passcode
          --print $ length passcode
          hClose h


{-shortestPasscode ts = shortestPasscode' [] (concat ts)
    where shortestPasscode' rs []     = rs
          shortestPasscode' rs (n:ns) = 
                let (ds, es) = break (== n) rs
                in -}

{-addTr :: [Int] -> [Int] -> [Int]
addTr []       ps = ps
addTr t@(n:ns) ps = let (ds, es) = break (== n) ps
                    in if null es 
                            then ps ++ t
                            else ds ++ [head es] ++ addTr ns (tail es)-}

addTr t@(a:b:c:[]) ps = let (lta, gea) = break (== a) ps in
                        if null gea 
                           then ps ++ t
                           else case elemIndex c gea of
                                    Nothing -> 
                           --ds ++ [head es] ++ addTr ns (tail es)

{-addTr t@(a:b:c:[]) ps = let ixa = fromMaybe (length ps) $ elemIndex a ps
                        if null gea 
                           then ps ++ t
                           else ds ++ [head es] ++ addTr ns (tail es)-}


shortestPasscode' :: [[Int]] -> [[Int]]
shortestPasscode' ts = addTriplet (head $ tail ts) [head ts]
    where addTriplet :: [Int] -> [[Int]] -> [[Int]]
          addTriplet [] vs = vs
{-          addTriplet t  vs = addTriplet (tail t)
                              $ concatMap (insertions (head t)) vs-}
          addTriplet t  vs = concatMap (mergings t) vs

mergings :: [Int] -> [Int] -> [[Int]]
mergings [] ls = [ls]
mergings es ls = concatMap (mergings (tail es)) 
                    $ insertions (head es) ls

insertions :: Int -> [Int] -> [[Int]]
insertions e ls = map (\(before, after) -> before ++ (e : after))
                    $ map (flip splitAt ls) [0..length ls]


-- [[3,1,9],[6,8,0]]
