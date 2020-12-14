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
import Data.Bifunctor (first,  Bifunctor(second) )
import Data.Bits ((.|.), setBit, Bits(clearBit))
import Data.List (foldl')

data Mod = Overwrite
         | Floating
         deriving (Show, Eq)

type Memory = M.Map Int Int
type Mask = [(Int, Mod)]
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
              '1' -> Overwrite
              'X' -> Floating
      s' = enumerate $ reverse s

  return $ map (second t) $ filter ((/= '0') . snd) s'

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

orBitmask :: Mask -> Int
orBitmask = foldl' setBit 0
            . map fst
            . filter ((== Overwrite) . snd)

floating :: Mask -> [[Int -> Int]]
floating = mapM ((\p -> [flip setBit p, flip clearBit p]) . fst)
           . filter ((== Floating) . snd)

applyMask :: Mask -> Cell -> [Cell]
applyMask mask (x, y) = zip xs $ repeat y
  where x' = x .|. orBitmask mask
        xs = map (foldl' (\x'' p -> p x'') x') $ floating mask

execPack :: Pack -> [Cell]
execPack (mask, cs) = concatMap (applyMask mask) cs

main :: IO ()
main = do
  let fileName = "in.txt"

  raw <- BS.readFile fileName

  let Right ps = parse packs fileName raw
      cs = concatMap execPack ps
      mem = M.fromList cs

  print $ sum $ M.elems mem
