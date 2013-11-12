data Dart = Miss | S Int | D Int | T Int
          deriving (Eq, Show)

score Miss = 0
score (S r) = r
score (D r) = 2 * r
score (T r) = 3 * r

regions = [1..20] ++ [25]

doubles = map D regions
allDarts = Miss : map S regions ++ doubles ++ map T (init regions)

sameOrAfter d = dropWhile (/= d) allDarts

checkouts = [ (a, b, c) | a <- allDarts, b <- sameOrAfter a, c <- doubles ]

scoreCheckout (a, b, c) = score a + score b + score c


main = print $
       length . filter (< 100) $ map scoreCheckout checkouts
-- 38182
