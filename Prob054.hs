import System.IO
import Data.List
import Data.Char (digitToInt)

main :: IO ()
main = do 
       inh <- openFile "poker.txt" ReadMode
       lines <- readAllLines inh
       let result = process lines
       print result
       hClose inh
-- 376


readAllLines :: Handle -> IO [String]
readAllLines inh = 
    do ineof <- hIsEOF inh
       if ineof
          then return []
          else do inpStr <- hGetLine inh
                  rest <- readAllLines inh
                  return (inpStr : rest)


process lines =
    length . filter (== GT) $ map (compareHands . pairOfHands) lines
    where pairOfHands = splitAt 5 . map card . words


cardValue v | v `elem` ['2'..'9'] 
              = digitToInt v
cardValue 'T' = 10
cardValue 'J' = 11
cardValue 'Q' = 12
cardValue 'K' = 13
cardValue 'A' = 14

card [v, s] = (cardValue v, s)


type Value = Int
data Hand = HighCard Value |
            OnePair Value |
            TwoPairs Value Value |
            ThreeOfAKind Value |
            Straight Value |
            Flush |
            FullHouse Value Value |
            FourOfAKind Value |
            StraightFlush |
            RoyalFlush
    deriving (Eq, Ord, Show)


rankHand h
    | isFlush h && consecutive h && highestValue h == 14
        = RoyalFlush
    | isFlush h && consecutive h
        = StraightFlush
    | (length . head $ valueGroups h) == 4
        = FourOfAKind (head . head $ valueGroups h)
    | (length . head $ valueGroups h) == 3 &&
      (length . head . tail $ valueGroups h) == 2
        = FullHouse (head . head $ valueGroups h)
                    (head . head . tail $ valueGroups h)
    | isFlush h
        = Flush
    | consecutive h
        = Straight (highestValue h)
    | (length . head $ valueGroups h) == 3
        = ThreeOfAKind (head . head $ valueGroups h)
    | (length . head $ valueGroups h) == 2 &&
      (length . head . tail $ valueGroups h) == 2
        = TwoPairs (head . head $ valueGroups h)
                   (head . head . tail $ valueGroups h)
    | (length . head $ valueGroups h) == 2
        = OnePair (head . head $ valueGroups h)
    | otherwise
        = HighCard (highestValue h)


isFlush hand = length (nub $ map snd hand) == 1

consecutive cards = length diffs == 1 && head diffs == 1
    where diffs  = nub $ zipWith (-) (tail $ sortedValues cards) 
                                     (sortedValues cards)

highestValue cards = maximum (map fst cards)
sortedValues cards = sort (map fst cards)

valueGroups cards = reverse . sortBy compGr . group $ sortedValues cards
    where compGr a b = compare (length a) (length b)


compareHands (a, b) = case compare (rankHand a) (rankHand b) of
                        EQ -> compare (values a) (values b)
                        c  -> c
    where values = reverse . sortedValues


-- Test data
phs = [([(8,'C'), (10,'S'), (13,'C'), (9,'H'),  (4,'S')],
        [(7,'D'), (2,'S'),  (5,'D'),  (3,'S'),  (4,'C')]),

       ([(5,'C'), (14,'D'), (5,'D'),  (14,'C'), (9,'C')],
        [(7,'C'), (5,'H'),  (8,'D'),  (10,'D'), (13,'S')])]
