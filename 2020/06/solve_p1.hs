#!/usr/bin/env runhaskell

import qualified Data.Set as S
import Data.List.Split ( splitOn )

main :: IO ()
main = do
  customsDeclarationsRaw <- splitOn [""] . lines <$> readFile "in.txt"

  let customsDeclarations = map (S.fromList . concat) customsDeclarationsRaw
      solution = sum $ map S.size customsDeclarations

  print solution
