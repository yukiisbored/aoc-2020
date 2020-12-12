#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

step' :: Int -> Int -> (Int, Int)
step' takeX y = ((takeX * y) `mod` 31, y)

navigate' :: (Int -> (Int, Int)) -> Int -> [(Int, Int)]
navigate' stepPred targetY = [stepPred y | y <- [1..targetY]]

firstCase :: Int -> [(Int, Int)]
firstCase = navigate' $ step' 1

secondCase :: Int -> [(Int, Int)]
secondCase = navigate' $ step' 3

thirdCase :: Int -> [(Int, Int)]
thirdCase = navigate' $ step' 5

fourthCase :: Int -> [(Int, Int)]
fourthCase = navigate' $ step' 7

fifthCase :: Int -> [(Int, Int)]
fifthCase targetY = [((y `div` 2) `mod` 31, y) | y <- [2,4..targetY]]

countTrees' :: [T.Text] -> (Int -> [(Int, Int)]) -> Int
countTrees' world naviPred  = (length . filter (== '#')) traversedTiles
  where points = naviPred (length world - 1)
        traversedTiles = map (\(x, y) -> T.index (world !! y) x) points

main :: IO ()
main = do
  world <- T.lines <$> TIO.readFile "in.txt"

  let countTrees = countTrees' world

      first = countTrees firstCase
      second = countTrees secondCase
      third = countTrees thirdCase
      fourth = countTrees fourthCase
      fifth = countTrees fifthCase

      solution = first * second * third * fourth * fifth

  print solution
