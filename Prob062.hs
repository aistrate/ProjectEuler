import Data.List (sort, sortBy, groupBy)
import Prob035 (makeNr)
import Prob038 (digitList)

cubes n = [ i * i * i | i <- [1..n] ]

permGroupings ns = map (sort . (map snd)) groups
    where groups = groupBy pairEq $ sortBy pairComp pairs
          pairs = zip (map (sort . digitList) ns) ns
          pairComp a b = compare (fst a) (fst b)
          pairEq a b = (fst a) == (fst b)


sortByLength ls = sortBy lenComp ls
    where lenComp a b = compare (length a) (length b)

filterByLength min = filter (\g -> length g >= min)


permCubes = reverse . sortByLength . longGroups
    where longGroups = (filterByLength 2) . permGroupings . cubes


main = print $
       minimum . map head $ filterByLength (length . head $ pc) pc
         where pc = permCubes 10000
-- 127035954683


-- [[140283769536,536178930624,613258407936,913237656408,936302451687],
--  [127035954683,352045367981,373559126408,569310543872,589323567104]]
