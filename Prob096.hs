import System.IO
import Data.Array.Diff
import Data.List (foldl', find, (\\), intersect, nub)
import Data.Maybe (fromJust)
import Data.Char (digitToInt)


-- Input/Output
main :: IO ()
main = do 
       lines <- readSudokuFile
       let puzzles = readPuzzles lines
       let solvedPuzzles = map (checkSol . solvePuzzle) puzzles
       print . sum $ map (getTopLeftNumber . head) solvedPuzzles
-- 24702    (about 16 sec.)

test :: IO ()
test = do 
       lines <- readSudokuFile
       let puzzles = take 50 . drop 0 $ readPuzzles lines
       --printPuzzles puzzles
       let solvedPuzzles = map solvePuzzle puzzles
       printSolutions solvedPuzzles

readSudokuFile :: IO [String]
readSudokuFile = do contents <- readFile "sudoku.txt"
                    return $ lines contents

readPuzzles :: [String] -> [[[Digit]]]
readPuzzles = map (readPuzzle . drop 1) . splitInto 10
    where readPuzzle = map (map digitToInt)

printWithSep :: Show a => String -> (a -> IO ()) -> [a] -> IO ()
printWithSep s printElem = mapM_ (((putStrLn s) >>) . printElem)

printPuzzles :: Show a => [[[a]]] -> IO ()
printPuzzles = printWithSep "" (mapM_ print)

printSolutions :: Show a => [[[[a]]]] -> IO ()
printSolutions = printWithSep ("\n" ++ replicate 22 '-') printPuzzles

printGrid :: Show a => Grid a -> IO ()
printGrid = mapM_ print . gridToList


-- Slot data type
type Digit = Int
data Slot = Fixed Digit |
            Options [Digit] |
            Undefined
    deriving (Eq)

instance Show Slot where
    show (Fixed d)    = show d
    show (Options ds) = show ds
    show Undefined    = show 0

makeSlot 0 = Undefined
makeSlot d = Fixed d

getDigit Undefined = 0
getDigit (Fixed d) = d

isFixed (Fixed _) = True
isFixed _         = False

fromFixed (Fixed d) = d

isOptions (Options _) = True
isOptions _           = False

fromOptions (Options ds) = ds


-- Grid data type
type GridIndex = (Int, Int)
type Grid a = Array GridIndex a

gridBounds :: (GridIndex, GridIndex)
gridBounds = ((1, 1), (9, 9)) 

makeSlotGrid :: [[Digit]] -> Grid Slot
makeSlotGrid = listArray gridBounds . map makeSlot . concat

gridToList :: Grid a -> [[a]]
gridToList = splitInto 9 . elems

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n xs = (take n xs) : (splitInto n (drop n xs))


-- extract 3 digits from top left corner
getTopLeftNumber :: [[Slot]] -> Int
getTopLeftNumber = makeNr . map getDigit . take 3 . head

makeNr :: [Digit] -> Int
makeNr = foldl' addDigit 0
         where addDigit s d = 10 * s + d


-- printable, checked solution
solvePuzzle :: [[Digit]] -> [[[Slot]]]
solvePuzzle = map gridToList . filter hasUniqueness 
                             . solveSudoku . makeSlotGrid

checkSol :: [[[Slot]]] -> [[[Slot]]]
checkSol xs | length xs == 1 && 
              all isValidSol xs = xs
            | otherwise         = error "Invalid solution"
    where isValidSol = all isFixed . concat



-- -----------------------------------
-- Actual solving of the Sudoku puzzle
-- -----------------------------------
solveSudoku :: Grid Slot -> [Grid Slot]
solveSudoku g =
    case minOptionCount newGrid of
        -1 -> [newGrid]
        0  -> []
        1  -> let replacedGrid = replaceUniqueOptions newGrid
              in if hasUniqueness replacedGrid
                    then solveSudoku replacedGrid
                    else []
        m  -> let (i, ds) = firstOptionWithCount m newGrid
              in concat $ map (replaceAndSolve i) ds
    where newGrid = calculateOptions g
          replaceAndSolve i d = solveSudoku 
                                    $ replaceWithFixed newGrid i d


-- calculate next-level grid
calculateOptions :: Grid Slot -> Grid Slot
calculateOptions g = generateGrid calcSlot
    where calcSlot i = case g ! i of
                            f@(Fixed _) -> f
                            _           -> Options $ calcOptions i
          calcOptions i@(r, c) = (rc ! r) `intersect` 
                                 (cc ! c) `intersect` 
                                 (sc ! (squareMapping ! i))
          (rc, cc, sc) = (comps rowRanges,
                          comps colRanges,
                          comps squareRanges)
          comps rs = generateArray (1, 9) 
                                   (rangeComplement g . (rs !))

rangeComplement :: Grid Slot -> GridRange -> [Digit]
rangeComplement g r = [1..9] \\ (fixedSlotsInRange g r)

fixedSlotsInRange g r = map fromFixed . filter isFixed $ map (g !) r


-- helpers: looking for solutions
replaceUniqueOptions :: Grid Slot -> Grid Slot
replaceUniqueOptions g = g // (map pair $ assocsByOptionCount 1 g)
    where pair (i, s) = (i, Fixed . head $ fromOptions s)

assocsByOptionCount n = filter (optionCountIs n . snd) . assocs
    where optionCountIs n s = (isOptions s) && 
                              (n == (length $ fromOptions s))

hasUniqueness g = all unique $ map (fixedSlotsInRange g) allRanges
    where unique ls = nub ls == ls

allRanges = concat $ map elems [rowRanges, colRanges, squareRanges]

replaceWithFixed :: Grid Slot -> GridIndex -> Digit -> Grid Slot
replaceWithFixed g i d = g // [(i, Fixed d)]

minOptionCount :: Grid Slot -> Int
minOptionCount g 
        | null optionCounts = -1
        | otherwise         = minimum optionCounts
    where optionCounts = map (length . fromOptions) 
                            . filter isOptions $ elems g

firstOptionWithCount :: Int -> Grid Slot -> (GridIndex, [Digit])
firstOptionWithCount n = pair . head . assocsByOptionCount n
    where pair (i, s) = (i, fromOptions s)


-- index ranges
type GridRange = [GridIndex]

rowRanges :: Array Int GridRange
rowRanges = generateArray (1, 9) (\r -> range ((r, 1), (r, 9)))

colRanges :: Array Int GridRange
colRanges = generateArray (1, 9) (\c -> range ((1, c), (9, c)))

squareRanges :: Array Int GridRange
squareRanges = generateArray (1, 9) squareRange
    where squareRange s = let startRow = 3 * ((s-1) `div` 3) + 1
                              startCol = 3 * ((s-1) `mod` 3) + 1
                          in range ((startRow, startCol), 
                                    (startRow + 2, startCol + 2))

squareMapping :: Grid Int
squareMapping = generateGrid getSquare
    where getSquare i = fst . fromJust . find ((i `elem`) . snd) 
                            $ assocs squareRanges


-- helpers
generateArray :: Ix i => (i, i) -> (i -> e) -> Array i e
generateArray bs f = array bs . map (\i -> (i, f i)) $ range bs

generateGrid :: (GridIndex -> a) -> Grid a
generateGrid = generateArray gridBounds
