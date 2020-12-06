#!/usr/bin/env runhaskell

import qualified Data.Set as S
import Data.List.Split ( splitOn )

main :: IO ()
main = do
  customsDeclarationsRaw <- splitOn [""] . lines <$> readFile "in.txt"

  let zero = S.fromList ['a'..'z']
      commonAnswers = map (foldr (S.intersection . S.fromList) zero)
                      customsDeclarationsRaw
      solution = sum $ map S.size commonAnswers

  print solution
