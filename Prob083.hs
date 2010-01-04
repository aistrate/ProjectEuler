import System.IO
import qualified Data.ByteString.Char8 as C8
import Data.Array
import Data.List
import Data.Maybe (fromJust)


main = do h <- openFile "matrix.txt" ReadMode
          c <- hGetContents h
          print . minimalPath $ readMatrix c
          hClose h

-- 18 sec interpreted, < 3 sec compiled
-- (425185,[(1,1),(2,1),(2,2),(2,3),(1,3),(1,4),(1,5),(1,6),(1,7),(2,7),(2,8),(3,8),(3,9),(4,9),(4,10),(4,11),(4,12),(5,12),(5,13),(5,14),(5,15),(5,16),(6,16),(7,16),(7,17),(7,18),(7,19),(6,19),(6,20),(6,21),(6,22),(6,23),(6,24),(7,24),(7,25),(7,26),(7,27),(7,28),(7,29),(7,30),(8,30),(9,30),(9,31),(10,31),(10,32),(11,32),(12,32),(12,31),(13,31),(14,31),(15,31),(16,31),(17,31),(18,31),(18,32),(18,33),(18,34),(18,35),(19,35),(20,35),(21,35),(22,35),(23,35),(23,36),(24,36),(25,36),(25,37),(25,38),(26,38),(26,39),(27,39),(27,40),(27,41),(27,42),(28,42),(28,43),(28,44),(29,44),(30,44),(31,44),(32,44),(33,44),(33,45),(33,46),(33,47),(34,47),(35,47),(36,47),(37,47),(38,47),(39,47),(40,47),(40,48),(40,49),(41,49),(42,49),(43,49),(44,49),(45,49),(45,50),(46,50),(47,50),(48,50),(48,51),(49,51),(49,52),(50,52),(50,53),(51,53),(51,54),(51,55),(51,56),(52,56),(53,56),(53,57),(53,58),(53,59),(54,59),(54,60),(54,61),(55,61),(56,61),(57,61),(58,61),(59,61),(60,61),(61,61),(62,61),(63,61),(63,62),(63,63),(63,64),(63,65),(63,66),(63,67),(64,67),(65,67),(66,67),(67,67),(68,67),(68,68),(68,69),(68,70),(68,71),(68,72),(69,72),(69,73),(70,73),(70,74),(70,75),(70,76),(70,77),(71,77),(72,77),(72,78),(73,78),(73,79),(74,79),(75,79),(76,79),(77,79),(78,79),(79,79),(80,79),(80,80)])


readMatrix :: String -> Matrix
readMatrix cs = let ls = map (map (read . C8.unpack) . C8.split ',' . C8.pack) $ lines cs
                    (rows, cols) = (length ls, length $ head ls)
                in listArray ((1, 1), (rows, cols)) $ concat ls


type CellIndex = (Int, Int)
type Matrix = Array CellIndex Int

data NodePath = NodePath { sumValue :: Int, path :: [CellIndex] }
    deriving (Eq, Show)

data SearchState = SearchState { visited :: [NodePath], touched :: [NodePath] }
    deriving Show


minimalPath :: Matrix -> (Int, [CellIndex])
minimalPath matrix = let (sourceIx, destIx) = bounds matrix
                         NodePath sumVal minPath = dijkstra matrix sourceIx destIx
                     in ((sumVal + matrix ! sourceIx + matrix ! destIx) `div` 2, 
                         reverse minPath)


dijkstra :: Matrix -> CellIndex -> CellIndex -> NodePath
dijkstra matrix sourceIx destIx
        = let initState = SearchState [] [NodePath 0 [sourceIx]]
              finalState = head . dropWhile (\st -> not . null $ touched st) 
                                $ iterate (dijkstraStep matrix) initState
          in fromJust . find (\node -> nodeIndex node == destIx) $ visited finalState


dijkstraStep :: Matrix -> SearchState -> SearchState
dijkstraStep matrix state = let currentNode = minimumBy (\p q -> compare (sumValue p) (sumValue q)) 
                                                      $ touched state
                                neighborIxs = neighbors matrix $ nodeIndex currentNode
                                visitedIxs = map nodeIndex $ visited state
                                newTouched = map (addToPath matrix currentNode) $ neighborIxs \\ visitedIxs
                            in SearchState (currentNode : visited state) 
                                           (addOrUpdateNodes newTouched (touched state \\ [currentNode]))


addOrUpdateNodes :: [NodePath] -> [NodePath] -> [NodePath]
addOrUpdateNodes newNodes oldNodes = 
    let existingNodes = intersectBy (\p q -> nodeIndex p == nodeIndex q) oldNodes newNodes
        minimalNode newNode = case find (\p -> nodeIndex p  == nodeIndex newNode) existingNodes of
                                Nothing      -> newNode
                                Just oldNode -> if sumValue newNode < sumValue oldNode
                                                    then newNode
                                                    else oldNode
    in (map minimalNode newNodes) ++ (oldNodes \\ existingNodes)


addToPath :: Matrix -> NodePath -> CellIndex -> NodePath
addToPath matrix node ix = let val = matrix ! ix + matrix ! (nodeIndex node)
                           in NodePath { sumValue = val + sumValue node, path = ix : path node }


neighbors matrix (row, col) = let candidates = [(row - 1, col), (row + 1, col), (row, col -1), (row, col + 1)]
                                  ((r1, c1), (r2, c2)) = bounds matrix
                              in filter (\(r, c) -> r1 <= r && r <= r2 && c1 <= c && c <= c2) candidates


nodeIndex :: NodePath -> CellIndex
nodeIndex = head . path


testMatrix :: Matrix
testMatrix = listArray ((1, 1), (5, 5)) 
   [ 131, 673, 234, 103,  18,
     201,  96, 342, 965, 150,
     630, 803, 746, 422, 111,
     537, 699, 497, 121, 956,
     805, 732, 524,  37, 331 ]
