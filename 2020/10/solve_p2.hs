#!/usr/bin/env runhaskell
import qualified Data.List as L

-- Based on the following proof by tgujar : https://tgujar.github.io/2020/12/10/AOCDay10.html

tri :: Int -> Int
tri 0 = 1
tri 1 = 1
tri 2 = 2
tri x = sum [tri $ x - 1, tri $ x - 2, tri $ x - 3]

main :: IO ()
main = do
  raw <- readFile "in.txt"

  let adapters = 0 : L.sort ( map read (lines raw)) :: [Int]
      diff     = zipWith (flip (-)) adapters (tail adapters) ++ [3]
      pos      = [fst x | x <- zip [1..] diff, snd x == 3]
      dists    = map pred $ zipWith (-) pos (0:pos)

  print $ product [tri x | x <- dists]
