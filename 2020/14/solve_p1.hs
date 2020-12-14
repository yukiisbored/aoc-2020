#!/usr/bin/env runhaskell

import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import Text.Parsec.ByteString (Parser)
import Text.Parsec
    (parse,  digit,
      oneOf,
      spaces,
      string,
      eof,
      many1,
      manyTill,
      (<|>),
      lookAhead,
      try )
import Data.Functor (($>))
import Data.Bifunctor ( Bifunctor(second) )
import Data.Bits (setBit, Bits(clearBit))
import Data.List (foldl')

type Memory = M.Map Int Int
type Mask = [(Int, Bool)]
type Cell = (Int, Int)
type Pack = (Mask, [Cell])

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

int :: Parser Int
int = do
  n <- many1 digit
  let num = read n :: Int
  return num

mask :: Parser Mask
mask = do
  s <- string "mask = " *> many1 (oneOf "10X") <* spaces

  let t x = case x of
              '1' -> True
              '0' -> False
      s' = enumerate $ reverse s

  return $ map (second t) $ filter ((/= 'X') . snd) s'

cell :: Parser Cell
cell = do
  addr <- string "mem[" *> int <* string "] = "
  n <- int <* spaces

  return (addr, n)

pack :: Parser Pack
pack = do
  m <- mask
  cs <- manyTill cell (try (lookAhead mask) <|> (eof $> []))

  return (m, cs)

packs :: Parser [Pack]
packs = spaces *> many1 pack <* spaces

applyMask :: Mask -> Int -> Int
applyMask mask x = foldl' (\x' (p, bit) -> apply bit x' p) x mask
  where apply x = if x then setBit else clearBit

execPack :: Pack -> [Cell]
execPack (mask, cs) = map (second (applyMask mask)) cs

main :: IO ()
main = do
  let fileName = "in.txt"

  raw <- BS.readFile fileName

  let Right ps = parse packs fileName raw
      cs = concatMap execPack ps
      mem = M.fromList cs

  print $ sum $ M.elems mem
