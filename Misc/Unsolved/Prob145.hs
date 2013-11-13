import Data.List (nub, (\\))
import Timer

rev :: Int -> Maybe Int
rev n = let r = reverse (show n)
        in if head r /= '0' then Just (read r) else Nothing

isReversible n = case (rev n) of 
                    Just r -> null $ (nub . show $ n + r) \\ ['1','3'..'9']
                    Nothing -> False


main = printTime . print . length . filter isReversible $ [1..10^7]
