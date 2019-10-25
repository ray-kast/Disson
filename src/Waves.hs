module Waves where

saw :: [(Float, Float)]
saw = f 1
  where f i = let i' = fromIntegral i in (i', 1.0 / i'):f (i + 1) :: [(Float, Float)]

nstate :: Integer -> [(Float, Float)]
nstate n = f 1
  where f i = if (i `mod` n) == 0 then rest else let i' = fromIntegral i in (i', 1.0 / i'):rest
          where rest = f (i + 1) :: [(Float, Float)]

square = nstate 2
tristate = nstate 3

