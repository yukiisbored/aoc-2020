#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Either (isRight, fromRight)
import Data.List (foldl')
import Control.Monad (zipWithM)

parse :: T.Text -> ([Int], [Int])
parse raw = foldl' (\(as, bs) (a, b) -> (a:as, b:bs)) ([], []) buses
  where buses = map ((\(i, x) -> (x - i, x)) . \(i, x) -> (i, fst $ fromRight (0, "") x))
                $ filter (isRight . snd)
                $ zipWith (\ i x -> (i, TR.decimal x)) [0..]
                $ T.splitOn ","
                $ head . tail
                $ T.lines raw

-- Taken from https://rosettacode.org/wiki/Chinese_remainder_theorem#Haskell

egcd :: Int -> Int -> (Int, Int)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b

modInv :: Int -> Int -> Either String Int
modInv a b =
  case egcd a b of
    (x, y)
      | a * x + b * y == 1 -> Right x
      | otherwise ->
        Left $ "No modular inverse for " ++ show a ++ " and " ++ show b

chineseRemainder :: [Int] -> [Int] -> Either String Int
chineseRemainder residues modulii =
  zipWithM modInv crtModulii modulii >>=
  (Right . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues)
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii

main :: IO ()
main = do
  raw <- TIO.readFile "in.txt"

  print $ uncurry chineseRemainder $ parse raw
