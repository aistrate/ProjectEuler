import System.IO
import Data.Array.Diff
import Data.List (foldl', (\\), intersect)
import Data.Char (digitToInt)

main = do contents <- readFile "sudoku.txt"
          let puzzles = readPuzzles $ lines contents
          let solvedPuzzles = map solvePuzzle puzzles
          print . sum $ map topLeftNumber solvedPuzzles
    where readPuzzles = map (readPuzzle . drop 1) . splitInto 10
          readPuzzle  = map (map digitToInt)
          solvePuzzle = gridToList . head . solveSudoku . makeSlotGrid
-- 24702 (about 16 sec.)

-- extract 3 digits from top left corner
topLeftNumber :: [[Slot]] -> Int
topLeftNumber = makeNum . map fromFixed . take 3 . head
    where makeNum = foldl' addDigit 0
          addDigit s d = 10 * s + d

-- Slot data type
type Digit = Int
data Slot = Fixed Digit |
            Options [Digit] |
            Undefined
    deriving (Eq)

makeSlot 0 = Undefined
makeSlot d = Fixed d

isFixed (Fixed _) = True
isFixed _         = False

fromFixed (Fixed d) = d

isOptions (Options _) = True
isOptions _           = False

optionCount (Options ds) = length ds

-- Grid data type
type GridIndex = (Int, Int)
type Grid a = Array GridIndex a

gridBounds :: (GridIndex, GridIndex)
gridBounds = ((1, 1), (9, 9)) 

makeSlotGrid :: [[Digit]] -> Grid Slot
makeSlotGrid = listArray gridBounds . map makeSlot . concat

gridToList :: Grid a -> [[a]]
gridToList = splitInto 9 . elems

-- solving the Sudoku puzzle
solveSudoku :: Grid Slot -> [Grid Slot]
solveSudoku g =
    case minOptionCount newGrid of
        -1 -> [newGrid]
        0  -> []
        m  -> let (i, Options ds) = firstOptionWithCount m newGrid
              in concat $ map (solveFor i) ds
    where newGrid = calculateOptions g
          solveFor i d = solveSudoku $ replaceWithFixed newGrid i d

-- calculate next-level grid
calculateOptions :: Grid Slot -> Grid Slot
calculateOptions g = generateGrid calcSlot
    where calcSlot i = if isFixed (g ! i) then (g ! i)
                                          else Options $ calcOptions i
          calcOptions i@(r, c) = (rc ! r) `intersect` (cc ! c) 
                                          `intersect` (sc ! (squareMapping ! i))
          (rc, cc, sc) = (comps rowRanges, comps colRanges, comps squareRanges)
          comps rs = generateArray (1, 9) (rangeComplement g . (rs !))

rangeComplement :: Grid Slot -> GridRange -> [Digit]
rangeComplement g r = [1..9] \\ fixedSlots
    where fixedSlots = map fromFixed . filter isFixed $ map (g !) r

-- helpers: looking for solutions
replaceWithFixed :: Grid Slot -> GridIndex -> Digit -> Grid Slot
replaceWithFixed g i d = g // [(i, Fixed d)]

minOptionCount :: Grid Slot -> Int
minOptionCount g | null counts = -1
                 | otherwise   = minimum counts
    where counts = map optionCount . filter isOptions $ elems g

firstOptionWithCount :: Int -> Grid Slot -> (GridIndex, Slot)
firstOptionWithCount n = head . filter (hasCount . snd) . assocs
    where hasCount s = isOptions s && optionCount s == n

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
                          in range ((startRow, startCol), (startRow + 2, startCol + 2))

squareMapping :: Grid Int
squareMapping = generateGrid getSquare
    where getSquare i = fst . head . filter ((i `elem`) . snd) $ assocs squareRanges

-- helpers
generateArray :: Ix i => (i, i) -> (i -> e) -> Array i e
generateArray bs f = array bs . map (\i -> (i, f i)) $ range bs

generateGrid :: (GridIndex -> a) -> Grid a
generateGrid = generateArray gridBounds

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n xs = (take n xs) : (splitInto n (drop n xs))
