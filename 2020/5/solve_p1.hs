#!/usr/bin/env runhaskell
import Data.Maybe ( mapMaybe )

subdivide :: [Bool] -> [Int] -> Maybe Int

subdivide [] [a] = Just a
subdivide [] _   = Nothing

subdivide (ins:tail) xs
  | ins     = subdivide tail upperHalf
  | not ins = subdivide tail lowerHalf
  where halfPoint              = length xs `div` 2
        (lowerHalf, upperHalf) = splitAt halfPoint xs

translate :: String -> [Bool]
translate = map translate'
  where translate' :: Char -> Bool
        translate' c
          | c == 'F' = False -- F means lower half, B means upper half
          | c == 'B' = True
          | c == 'L' = False  -- L means lower half, R means upper half
          | c == 'R' = True

find' :: [Int] -> String -> Maybe Int
find' xs cmd = subdivide translated xs
  where translated = translate cmd

findRow :: String -> Maybe Int
findRow = find' [0..127]

findColumn :: String -> Maybe Int
findColumn = find' [0..7]

find :: String -> (Maybe Int, Maybe Int)
find cmd = (findRow rowCmd, findColumn columnCmd)
  where (rowCmd, columnCmd) = splitAt 7 cmd

seatId' :: (Maybe Int, Maybe Int) -> Maybe Int
seatId' (Just row, Just column) = Just $ row * 8 + column
seatId' _                       = Nothing

seatId :: String -> Maybe Int
seatId = seatId' . find

main :: IO ()
main = do
  passes <- lines <$> readFile "in.txt"

  let seatIds = mapMaybe seatId passes

  print $ maximum seatIds
