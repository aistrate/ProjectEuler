spiralSum n = 1 + 4 * n + 2 * (n * (n + 1) * (8 * n + 7)) `div` 3

result = spiralSum 500
-- 669171001
