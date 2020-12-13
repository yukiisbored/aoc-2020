#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Either (rights)
import Data.List (minimumBy)
import Data.Ord (comparing)


parse :: T.Text -> (Int, [Int])
parse raw = (time, buses)
  where l = T.lines raw
        Right (time, "") = TR.decimal $ head l
        buses = map fst
                $ rights
                $ map TR.decimal
                $ filter (/= "x")
                $ T.splitOn ","
                $ l !! 1

solve :: (Int, [Int]) -> Int
solve (time, buses) = uncurry (*) $ minimumBy (comparing snd) buses'
  where buses' = map (\x -> (x, x - (time `mod` x))) buses

main :: IO ()
main = do
  raw <- TIO.readFile "in.txt"

  print $ solve $ parse raw
