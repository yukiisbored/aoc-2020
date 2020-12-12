#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import Data.List.Split ( splitOn )

requiredKeys :: [T.Text]
requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValid :: M.Map T.Text T.Text -> Bool
isValid passport = all (`M.member` passport) requiredKeys

main :: IO ()
main = do
  passportsRaw <- splitOn [""] . T.lines <$> TIO.readFile "in.txt"

  let passports = map (M.fromList .
                       map ((\[k,v] -> (k,v)) . T.splitOn ":") .
                       concatMap T.words)
                  passportsRaw

      solution = (length . filter isValid) passports

  print solution
