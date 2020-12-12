#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Map.Strict as M
import Data.List.Split ( splitOn )
import Data.Either ( isRight )

type Passport = M.Map T.Text T.Text

requiredKeys :: [T.Text]
requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

requiredFieldPresent :: Passport -> Bool
requiredFieldPresent p = all (`M.member` p) requiredKeys

birthYearValid :: Passport -> Bool
birthYearValid p = birthYear >= 1920 && birthYear <= 2002
  where Right (birthYear, "") = TR.decimal $ p M.! "byr"

issueYearValid :: Passport -> Bool
issueYearValid p = issueYear >= 2010 && issueYear <= 2020
  where Right (issueYear, "") = TR.decimal $ p M.! "iyr"

expirationYearValid :: Passport -> Bool
expirationYearValid p = expirationYear >= 2020 && expirationYear <= 2030
  where Right (expirationYear, "") = TR.decimal $ p M.! "eyr"

heightValid :: Passport -> Bool
heightValid p
  | unit == "in"  = height >= 59  && height <= 76
  | unit == "cm"  = height >= 150 && height <= 193
  | otherwise     = False
  where Right (height, unit) = TR.decimal $ p M.! "hgt"

hairColorValid :: Passport -> Bool
hairColorValid p = prefix == "#" && T.length color == 6 && isRight (TR.hexadecimal color)
  where (prefix, color) = T.splitAt 1 $ p M.! "hcl"

validColors :: [T.Text]
validColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

eyeColorValid :: Passport -> Bool
eyeColorValid p = color `elem` validColors
  where color = p M.! "ecl"

passportIDValid :: Passport -> Bool
passportIDValid p = T.length passportID == 9 && isRight (TR.decimal passportID)
  where passportID = p M.! "pid"

isValid :: Passport -> Bool
isValid p = and $ tests <*> pure p
  where tests = [requiredFieldPresent,
                 birthYearValid, issueYearValid, expirationYearValid,
                 heightValid,
                 hairColorValid,
                 eyeColorValid,
                 passportIDValid]

main :: IO ()
main = do
  passportsRaw <- splitOn [""] . T.lines <$> TIO.readFile "in.txt"

  let passports = map (M.fromList .
                       map ((\[k,v] -> (k,v)) . T.splitOn ":") .
                       concatMap T.words)
                  passportsRaw

      solution = (length . filter isValid) passports

  print solution
