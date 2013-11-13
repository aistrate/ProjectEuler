import Data.List (nub)

combinations n = [ (a, b) | a <- [2..n], b <- [2..n] ]

distinctTerms n = nub [ a ^ b | (a, b) <- combinations n ]

main = print $
       length $ distinctTerms 100
-- 9183
