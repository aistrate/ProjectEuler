import Data.List

palindromes :: [Int]
palindromes = [ z | x <- [999, 998..100], y <- [999, 998..100], 
                    let z = x * y, isPalindrome z]
    where isPalindrome a = let s    = show a
                               hlen = length s `div` 2
                           in take hlen s == take hlen (reverse s)


largestPalindrome = maximum palindromes
-- 906609 = 993 * 913


distinctPalindromes = reverse . map head . group . sort $ palindromes
