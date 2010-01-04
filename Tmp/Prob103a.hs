import Data.List (sort, group, (\\))
import Data.Array (Array, listArray, (!))
import Prob012 (allCombinations, combinations, makeUnique)


candidate 1 = [1]
candidate n = let prevCand = candidate (n - 1)
                  mid = head $ drop (length prevCand `div` 2) prevCand
              in mid : map (+ mid) prevCand

candidates :: Array Int [Int]
candidates = listArray (1, 7) $ map candidate [1..7]


optimalSets :: [Int] -> [[Int]]
optimalSets = ruleTwoFilter . ruleOneFilter . alternatives


alternatives :: [Int] -> [[Int]]
alternatives cand = let candSum = sum cand
                        interv k l m = filter (> 0) [m-k..m+l]
                    in makeUnique . map sort
                     . filter ((<= candSum) . sum) 
                     . sequence 
                     $ interv 4 1 (head cand) : map (interv 7 2) (tail cand)


ruleOneFilter :: [[Int]] -> [[Int]]
ruleOneFilter alts = let n = length $ head alts
                         indexCombs = allCombinations [1..n]
                         subsets idxCombs set = let setArr = listArray (1, n) set
                                                in map (map (setArr !)) idxCombs
                         equalSubsets = any (> 1) . map length . group . sort . map sum
                     in map fst
                      . filter (not . equalSubsets . snd) 
                      $ map (\alt -> (alt, subsets indexCombs alt)) alts


ruleTwoFilter :: [[Int]] -> [[Int]]
ruleTwoFilter alts = let n = length $ head alts
                         disjSubsetsIxs = disjointSubsets [1..n]
                         disjSubsets ixs set = let setArr = listArray (1, n) set
                                               in map (\(s1, s2) -> (map (setArr !) s1, map (setArr !) s2)) ixs
                         greaterSnds = any (\(s1, s2) -> sum s1 <= sum s2)
                     in map fst
                      . filter (not . greaterSnds . snd) 
                      $ map (\alt -> (alt, disjSubsets disjSubsetsIxs alt)) alts


-- first subset should have more elements than the second
disjointSubsets :: Ord a => [a] -> [([a], [a])]
disjointSubsets set = let combinationsTo k xs = concat [ combinations i xs | i <- [1..k] ]
                          subsetPairs comb = map (\ds -> (comb, ds))
                                           $ combinationsTo (length comb - 1) (set \\ comb)
                      in concatMap subsetPairs . combinationsTo (length set - 1) $ set


main = do let optSets = optimalSets $ candidates ! 7
          let bestOptSets = let minSum = minimum $ map sum optSets
                            in filter ((== minSum) . sum) optSets
          mapM_ putStrLn $ map (concat . (map show)) bestOptSets
-- Compiled (80 sec.):
-- 20313839404245


test1 = mapM_ print $ map (optimalSets . (candidates !)) [1..6]

test2 = do let optSets = optimalSets $ candidates ! 7
           mapM_ print optSets
           print $ map sum optSets
           print $ length optSets
{- Compiled (80 sec.):
[20,31,38,39,40,42,45]
[20,32,39,40,41,43,46]
[20,33,40,41,42,44,47]
[20,34,37,39,40,41,48]
[20,35,38,40,41,42,49]
[21,32,39,40,41,43,46]
[21,33,40,41,42,44,47]
[21,35,38,40,41,42,49]
[22,33,39,42,44,45,46]
[22,33,40,41,42,44,47]
[255,261,267,259,265,262,268,266,271,269]
10
-}
