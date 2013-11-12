import System.IO
import qualified Data.ByteString.Char8 as B
import Data.Bits (xor)
import Data.Word (Word8)
import Data.Char (chr, ord)
import Data.List (find, isInfixOf)

main :: IO ()
main = do 
       inh <- openFile "cipher1.txt" ReadMode
       inpStr <- hGetContents inh
       let decoded = process inpStr
       print decoded
       print $ messageSum decoded
       hClose inh
-- 107359


messageSum = sum . map ord


process = decryptFindKey . makeList

makeList :: String -> [Word8]
makeList = map read . map B.unpack . B.split ',' . 
                B.pack . filter (`elem` ',':['0'..'9'])


decrypt ws key = zipWith xor ws (cycle key)

decryptFindKey ws = toChars . head $ filter checkCandidate candidates
    where candidates = map (decrypt ws) (map toWord8s $ keys 3)
          checkCandidate c = all (`isInfixOf` c) common
          common = map toWord8s mostCommonWords


toChars = map (chr . fromIntegral)

toWord8s :: [Char] -> [Word8]
toWord8s = map (fromIntegral . ord)

keys 0 = [[]]
keys n = [ a:ks | a <- ['a'..'z'], ks <- keys (n - 1) ]

-- it even works with "with" alone
mostCommonWords = ["the", "of", "with"]


-- Key: "god"
{- Formatted:
"(The Gospel of John, chapter 1)
 1 In the beginning the Word already existed. He was with God, and he 
   was God. 
 2 He was in the beginning with God. 
 3 He created everything there is. Nothing exists that he didn't make. 
 4 Life itself was in him, and this life gives light to everyone. 
 5 The light shines through the darkness, and the darkness can never 
   extinguish it. 
 6 God sent John the Baptist 
 7 to tell everyone about the light so that everyone might believe 
   because of his testimony. 
 8 John himself was not the light; he was only a witness to the light. 
 9 The one who is the true light, who gives light to everyone, was 
   going to come intothe world. 
10 But although the world was made through him, the world didn't 
   recognize him when he came. 
11 Even in his own land and among his own people, he was not accepted. 
12 But to all who believed him and accepted him, he gave the right to 
   become children of God. 
13 They are reborn! This is not a physical birth resulting from human 
   passion or plan, this rebirth comes from God.
14 So the Word became human and lived here on earth among us. He was 
   full of unfailing love andfaithfulness. And we have seen his glory, 
   the glory of the only Son of the Father."
-}
