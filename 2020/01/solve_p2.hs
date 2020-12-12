#!/usr/bin/env runhaskell

main :: IO ()
main = do
  content <-  readFile "in.txt"

  let nums = map (\x -> read x :: Int) $ lines content
      solution = head [x * y * z | x <- nums, y <- nums, z <- nums, x + y + z == 2020]

  print solution
