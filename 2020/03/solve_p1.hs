#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

moveTo :: Int -> (Int, Int)
moveTo y = (3 * y `mod` 31, y)

navigateTo :: Int -> [(Int, Int)]
navigateTo targetY = [moveTo y | y <- [1..targetY]]

main :: IO ()
main = do
  world <- T.lines <$> TIO.readFile "in.txt"

  let points = navigateTo (length world - 1)
      traversedTiles = map (\(x, y) -> T.index (world !! y) x) points
      solution = (length . filter (== '#')) traversedTiles

  print solution
