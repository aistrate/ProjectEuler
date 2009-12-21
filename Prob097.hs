import Data.Word


nonMersennePrime = 28433 * 2 ^ 7830457 + 1

result0 = nonMersennePrime `mod` (10 ^ 10)
-- 8739992577   (1 sec)


moduloPower :: Word64 -> Int -> Word64 -> Word64
moduloPower n k m = moduloPower' 1 k
    where moduloPower' acc 0 = acc
          moduloPower' acc p = let acc' = (n * acc) `mod` m
                                   p'   = p - 1
                               in seq acc' (seq p' (moduloPower' acc' p'))


result = let m = 10 ^ 10
         in (28433 * (moduloPower 2 7830457 m) + 1) `mod` m
-- 8739992577   (32 sec)
