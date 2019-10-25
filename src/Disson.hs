module Disson where

data Tone = Tone { a :: Double, f :: Double }

dissonSine f1 f2 = x * exp (1 - x)
  where x = abs (f2 - f1)

pairs []     = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs

disson :: (f -> Float) -> (a -> Float) -> [(f, a)] -> Float
disson freqScale ampScale partials =
  sum [a1 * a2 * dissonSine f1 f2 | ((f1, a1), (f2, a2)) <- pairs partials']
  where partials' = [(freqScale f, ampScale a) | (f, a) <- partials]

mapTones :: [([(Float, Float)], Float)] -> [(Float, Float)]
mapTones [] = []
mapTones ((tone, offs):ts) = [(f * offs, a) | (f, a) <- tone] ++ mapTones ts

dissonTones freqScale ampScale tones = disson freqScale ampScale $ mapTones tones

