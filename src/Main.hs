module Main where

import Control.Monad
import Disson
import Waves
import Util
import System.IO

main :: IO ()
main = do table <- genRows 0
          let table' = postprocess table
          printTable table'
          hPutStr stderr "\r\x1b[2K"
  where resX = 1000 :: Integer
        resY = 1000 :: Integer

        mapX = relerp 0.0 (fromIntegral $ resX - 1) 0.0 2.0 . fromIntegral
        mapY = relerp 0.0 (fromIntegral $ resY - 1) 0.0 2.0 . fromIntegral

        tone1' = saw
        tone2' = saw
        tone3' = saw

        tone1 = take 32 tone1'
        tone2 = take 32 tone2'
        tone3 = take 32 tone3'

        ftoe f = 11.17268 * log (1.0 + (f * 46.06538) / (f + 14678.49))

        dt = dissonTones (\f -> 12.0 * ftoe (f * 440)) (\a -> a)

        genRows :: Int -> IO [[Float]]
        genRows r
          | r < resY = do row <- genRow r 0
                          rest <- genRows $ r + 1
                          return (row:rest)
          | otherwise = return []

        genRow :: Int -> Int -> IO [Float]
        genRow r c
          | c < resX = do cell <- genCell r c
                          rest <- genRow r $ c + 1
                          return (cell:rest)
          | otherwise = return []

        genCell :: Int -> Int -> IO Float
        genCell r c = do if (r + c) `mod` 100 == 0
                           then hPutStr stderr $ "\r\x1b[2Kx, y = " ++ show x ++ ", " ++ show y
                           else return ()
                         return $! dt [(tone1, 1.0), (tone2, 2.0 ** x), (tone3, 2.0 ** y)]
          where x = mapX c
                y = mapY r

        postprocess rows = normalized
          where least = let (x:xs) = join rows in foldr min x xs
                relative = map (\r -> map (\c -> logBase 2.0 (c / least)) r) rows
                most = let (x:xs) = join relative in foldr max x xs
                normalized = map (\r -> map (\c -> c / most) r) relative

        printTable rows = do printTableHead (head rows) 0
                             printTableBody rows 0

        printTableHead [] _ = putStr "\n"
        printTableHead (_:xs) c = do if c == 0
                                       then putStr "x/y/d"
                                       else return ()
                                     putStr $ "," ++ (show $ mapX c)
                                     printTableHead xs $ c + 1

        printTableBody [] _ = return ()
        printTableBody (row:rs) r = do printTableRow row r 0
                                       printTableBody rs $ r + 1

        printTableRow [] _ _ = putStr "\n"
        printTableRow (x:xs) r c = do if c == 0
                                        then putStr $ show $ mapY r
                                        else return ()
                                      putStr $ "," ++ show x
                                      printTableRow xs r $ c + 1

