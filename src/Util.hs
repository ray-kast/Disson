module Util where

lerp a b x = a + x * (b - a)

rlerp a b x = (x - a) / (b - a)

relerp a b a' b' = lerp a' b' . rlerp a b

