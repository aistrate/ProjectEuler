import Prob035 (makeNr)
import Prob038 (digitList)
import Prob036 (isPalindrome)

reverseAdd n = n + (makeNr . reverse $ digitList n)

isLychrel n = isLychrel' (reverseAdd n) 50
    where isLychrel' k _ | isPalindrome (digitList k) 
                             = False
          isLychrel' _ 1     = True
          isLychrel' k i     = isLychrel' (reverseAdd k) (i - 1)

lychrelNumbers = [ n | n <- [1..], isLychrel n ]


main = print $
       length $ takeWhile (< 10000) lychrelNumbers
-- 249
