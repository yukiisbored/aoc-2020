#!/usr/bin/env runhaskell
import qualified Data.List as L

solve :: [Int] -> Int
solve adapters = threes * ones
  where diff   = zipWith (flip (-)) adapters (tail adapters)
        threes = succ $ length $ filter (== 3) diff
        ones   = succ $ length $ filter (== 1) diff

main :: IO ()
main = do
  raw <- readFile "in.txt"

  let adapters = L.sort $ map read $ lines raw :: [Int]

  print $ solve adapters
