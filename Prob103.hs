import Data.List (sort, group, (\\))
import Data.Array (Array, listArray, (!))
import Prob012 (allCombinations, combinations, makeUnique)


optimals :: Array Int [Int]
optimals = listArray (1, 7) $ map bestOptimalSet [1..7]


bestOptimalSet :: Int -> [Int]
bestOptimalSet n = let optSets = optimalSets $ candidate n
                       minSum = minimum $ map sum optSets
                   in head . sort . filter ((== minSum) . sum) $ optSets


candidate 1 = [1]
candidate n = let prevCand = optimals ! (n - 1)
                  mid = head $ drop (length prevCand `div` 2) prevCand
              in mid : map (+ mid) prevCand


optimalSets :: [Int] -> [[Int]]
optimalSets = ruleTwoFilter . ruleOneFilter . alternatives


alternatives cand = let candSum = sum cand
                        interv k l m = filter (> 0) [m-k..m+l]
                    in filter ((<= candSum) . sum)
                     . alternatives' 0
                     $ interv 3 1 (head cand) : map (interv 8 5) (tail cand)
    where alternatives' minN []  = [[]] 
          alternatives' minN nns = [ n:rs | n <- filter (>= minN) (head nns), 
                                            rs <- alternatives' (n + 1) (tail nns) ]


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


main = putStrLn . concat . map show $ optimals ! 7
-- Compiled (100 sec.):
-- 20313839404245


test1 = mapM_ print $ map (optimals !) [1..6]

test2 = do let optSets = optimalSets $ candidate 7
           mapM_ print optSets
           print $ map sum optSets
           print $ length optSets
-- [20,31,38,39,40,42,45]
-- [255]
-- 1
