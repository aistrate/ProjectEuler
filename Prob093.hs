import Control.Monad (replicateM)
import Data.Ratio
import Data.List
import Data.Char (intToDigit)
import Prob012 (combinations)


-- Operations
data BinaryOp a = BinaryOp { action :: a -> a -> [a], symb :: String }

instance Eq (BinaryOp a) where
    op1 == op2 = symb op1 == symb op2

instance Show (BinaryOp a) where
    show = symb

ops :: [BinaryOp (Ratio Int)]
ops = [BinaryOp safeAdd "+", 
       BinaryOp safeSub "-",
       BinaryOp safeMul "*",
       BinaryOp safeDiv "/"]

safeAdd a b = [a + b]
safeSub a b = [a - b]
safeMul a b = [a * b]
safeDiv a b | b /= 0    = [a / b]
            | otherwise = []


-- Expression trees
data Exp = ExpVar Int
         | ExpOp Int Exp Exp
    deriving (Eq, Show)

allExps :: [Int] -> [Int] -> [Exp]
allExps []  [v]  = [ExpVar v]
allExps ops vars = let opPartitions = map (\i -> (i, take (i-1) ops, drop i ops)) 
                                          [1..length ops]
                       exps (rootIx, beforeOps, afterOps) = 
                            let root = ops !! (rootIx - 1)
                            in [ ExpOp root b a | b <- allExps beforeOps (take rootIx vars),
                                                  a <- allExps afterOps  (drop rootIx vars) ]
                   in concatMap exps opPartitions

showExp :: (Show a, Show b) => [a] -> [b] -> Exp -> String
showExp vars ops (ExpVar varIx)         = show $ vars !! (varIx - 1)
showExp vars ops (ExpOp opIx expL expR) = "(" ++ (showExp vars ops expL) ++ " "
                                              ++ (show $ ops !! (opIx - 1)) ++ " "
                                              ++ (showExp vars ops expR) ++ ")"

showAllExps n = map (showExp [1..n+1] (take n $ cycle ops)) $ allExps [1..n] [1..n+1]


-- Specific inputs
threeOpSets = replicateM 3 ops

threeOpExps = allExps [1..3] [1..4]

fourVarSets :: [[Int]]
fourVarSets = combinations 4 [1..9]


-- Solving the problem
calcExp :: [a] -> [BinaryOp a] -> Exp -> [a]
calcExp vars ops (ExpVar varIx)         = [vars !! (varIx - 1)]
calcExp vars ops (ExpOp opIx expL expR) = do left  <- calcExp vars ops expL
                                             right <- calcExp vars ops expR
                                             let op = action (ops !! (opIx - 1))
                                             op left right

calculateAll :: [Int] -> [Int]
calculateAll vars = map head . group . sort
                  . map numerator
                  . filter ((== 1) . denominator)
                  . filter (> 0) 
                  $ concat [ calcExp varPerm ops exp | varPerm <- permutations $ map (% 1) vars,
                                                       ops <- threeOpSets, 
                                                       exp <- threeOpExps ]


longestChain :: [Int] -> Int
longestChain []       = 0
longestChain (x:y:ys) | x + 1 == y = 1 + longestChain (y:ys)
                      | otherwise  = 1


list2Int :: [Int] -> Int
list2Int = read . map intToDigit

result = let candidates = map (\varSet -> (list2Int varSet, longestChain $ calculateAll varSet)) fourVarSets
         in sort . head . reverse
                 . groupBy (\c d -> (snd c) == (snd d))
                 . sortBy (\c d -> compare (snd c) (snd d)) 
                 $ candidates
-- [(1258,51)]      (25 sec.)


main = print result
-- (compiled: < 3 sec.)
