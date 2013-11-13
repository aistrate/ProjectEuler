import Data.List (foldl', permutations, nub)

makeNr = foldl' addDigit 0
         where addDigit s d = 10 * s + d

splitPerm m n p = (makeNr $ take m p,
                   makeNr $ take n (drop m p),
                   makeNr $ drop (m + n) p)

isPandigital m n p = a * b == c
                     where (a, b, c) = splitPerm m n p

findPandigitals m n ps = map (splitPerm m n) $ filter (isPandigital m n) ps


allPandigitals = findPandigitals 1 4 perms ++ findPandigitals 2 3 perms
                 where perms = permutations [1..9]

main = print $
       sum . nub $ map prod allPandigitals
         where prod (_, _, c) = c
-- 45228
