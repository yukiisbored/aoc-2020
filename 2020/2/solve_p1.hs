#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read

data PasswordCheck = Check Int Int Char T.Text
                   deriving (Show)

isValid :: PasswordCheck -> Bool
isValid (Check low high ch t)  = count >= low && count <= high
  where count = (T.length . T.filter (== ch)) t

parse :: T.Text -> PasswordCheck
parse s = Check low high ch password
  where xs = T.words s
        [boundT, charT, password] = xs
        [Right (low, _), Right (high, _)] = map decimal (T.splitOn "-" boundT)
        ch = T.head charT

main :: IO ()
main = do
  entries <- T.lines <$> TIO.readFile "in.txt"

  let res = (length . filter id) $ map (isValid . parse) entries

  print res
