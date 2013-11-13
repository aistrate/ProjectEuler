import System.IO
import Data.Char (ord)
import Data.List (sort)


main :: IO ()
main = do 
       inh <- openFile "names.txt" ReadMode
       inpStr <- hGetContents inh
       let result = process inpStr
       print result
       hClose inh
-- 871198282


process inp = process' names 1 0
              where names = sort $ read $ "[" ++ inp ++ "]"
                    process' []     _ acc = acc
                    process' (n:ns) i acc = process' ns (i + 1) 
                                                        (acc + i * nameValue n)


nameValue = sum . map letterValue
            where letterValue c = ord c - ord 'A' + 1
