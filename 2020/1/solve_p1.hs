#!/usr/bin/env runhaskell

main :: IO ()
main = do
  content <- readFile "in.txt"

  let nums = map (\x -> read x :: Int) $ lines content
      solution = head [x * y | x <- nums, y <- nums, x + y == 2020]

  (putStrLn . show) solution
