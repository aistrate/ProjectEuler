import System.IO
import Data.List
import Data.Maybe (fromJust)


main :: IO ()
main = do 
       h <- openFile "words.txt" ReadMode
       c <- hGetContents h
       let ws = read $ "[" ++ c ++ "]" :: [String]
     {-let result = allAnagramMatches ws
       mapM_ print result
       print $ length result-}
       let result = greatestAnagramMatches ws
       print result
       print . maxSquare $ head result
       hClose h
-- [(("BOARD",17689),("BROAD",18769))]
-- 18769


type Match = ((String, Integer), (String, Integer))

greatestAnagramMatches :: [String] -> [Match]
greatestAnagramMatches = last . groupBy' maxSquare . sortBy' maxSquare . allAnagramMatches

maxSquare :: Match -> Integer
maxSquare ((_, i1), (_, i2)) = max i1 i2


allAnagramMatches :: [String] -> [Match]
allAnagramMatches = concatMap matchAnagramGroups . groupByWordLen . anagrams

groupByWordLen :: [[String]] -> [[[String]]]
groupByWordLen = map (map fst) . groupBy' snd . sortBy' snd . map (\a -> (a, length $ head a))


-- all words in input must have the same length
matchAnagramGroups :: [[String]] -> [Match]
matchAnagramGroups wss = let wordLen = length . head $ head wss
                             squareAnagrams = digitAnagrams $ squaresOfLen wordLen
                             matches p@(w1, w2) = map (\(i1, i2) -> ((w1, i1), (w2, i2))) 
                                                    $ matchAnagramPair squareAnagrams p
                         in concatMap matches $ allPairs wss

matchAnagramPair :: [[Integer]] -> (String, String) -> [(Integer, Integer)]
matchAnagramPair sqAnagrams p@(w1, w2) = let sgn = signature w1
                                             candidates = filter ((== sgn) . signature . show . head) sqAnagrams
                                         in concatMap (matchPairWithGroup p) candidates

matchPairWithGroup :: (String, String) -> [Integer] -> [(Integer, Integer)]
matchPairWithGroup (w1, w2) ns = let ns' = map show ns
                                     createSubst = zip
                                     isValidSubst = all (== 1) . map (length . nub . map snd) . groupBy' fst . sortBy' fst
                                     substitute s w = map (fromJust . flip lookup s) w
                                     match n' = let s = createSubst w1 n'
                                                in if isValidSubst s then map (\n2' -> (read n', read n2')) 
                                                                                $ filter (`elem` (ns' \\ [n'])) [substitute s w2]
                                                                     else []
                                 in concatMap match ns'

allPairs :: Eq a => [[a]] -> [(a, a)]
allPairs = concatMap pairs
    where pairs [w1, w2] = [(w1, w2)]
          pairs (w:ws) = (map (\w' -> (w, w')) ws) ++ pairs ws

signature :: Ord a => [a] -> [Int]
signature = reverse . sort . map length . group . sort


anagrams = anagrams' id

digitAnagrams :: [Integer] -> [[Integer]]
digitAnagrams = anagrams' show

anagrams' :: Ord b => (a -> [b]) -> [a] -> [[a]]
anagrams' f = map (map fst)
            . filter (\g -> length g > 1)
            . groupBy' snd . sortBy' snd
            . map (\w -> (w, sort $ f w))


squaresOfLen :: Int -> [Integer]
squaresOfLen n = let minBound = ceiling . sqrt $ 10 ^ (n - 1)
                     maxBound = floor . sqrt $ 10 ^ n - 1
                 in map (^2) [minBound..maxBound]


sortBy' :: Ord b => (a -> b) -> [a] -> [a]
sortBy' f = sortBy (\p q -> compare (f p) (f q))

groupBy' :: Eq b => (a -> b) -> [a] -> [[a]]
groupBy' f = groupBy (\p q -> f p == f q)
