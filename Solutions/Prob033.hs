import Data.Ratio
import Data.Maybe
import Data.List

number (a, b) = 10 * a + b

numerators = [ (a, b) | a <- [1..9], b <- [1..9] ]

denominators (a, b) = [ (c, d) | (c, d) <- numerators, 
                                 number (a, b) < number (c, d),
                                 c `elem` [a, b] || d `elem` [a, b] ]

fractions = [ (num, den) | num <- numerators, den <- denominators num ]


curiousSimplify ((a, b), (c, d)) = 
        nub $ catMaybes [simplifyBy a, simplifyBy b]
    where simplifyBy m | canSimplifyBy m = Just ((head $ delete m [a, b]) %
                                                 (head $ delete m [c, d]))
                       | otherwise       = Nothing
          canSimplifyBy m = m `elem` [a, b] && m `elem` [c, d]


makeRatio ((a, b), (c, d)) = number (a, b) % number (c, d)

curiousFractions = filter isCurious fractions
    where isCurious frac = any (makeRatio frac ==) (curiousSimplify frac)
-- [((1,6),(6,4)), ((1,9),(9,5)), ((2,6),(6,5)), ((4,9),(9,8))]


main = print $
       denominator . product $ map makeRatio curiousFractions
-- 100
