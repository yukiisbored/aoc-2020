#!/usr/bin/env runhaskell

import Text.Parsec
    ( char,
      digit,
      letter,
      spaces,
      string,
      endBy,
      eof,
      many1,
      optional,
      sepBy,
      (<|>),
      parse,
      try )
import Text.Parsec.String ( Parser )
import Data.Graph.DGraph ( DGraph, fromArcsList, outboundingArcs )
import Data.Graph.Types ( Arc(Arc) )

int :: Parser Int
int = do
  n <- many1 digit

  return (read n)

bagName :: Parser String
bagName = do
  adj <- many1 letter <* spaces
  color <- many1 letter <* spaces <* string "bag" <* optional (char 's')

  return $ adj <> " " <> color

dest :: Parser (String, Int)
dest = do
  amount <- int <* spaces
  name <- bagName

  return (name, amount)

dests :: Parser [(String, Int)]
dests = try ([] <$ string "no other bags") <|> dest `sepBy` string ", "

rule :: Parser (String, [(String, Int)])
rule = do
  name <- bagName <* spaces <* string "contain" <* spaces
  destinations <- dests <* char '.'

  return (name, destinations)

rules :: Parser [(String, [(String, Int)])]
rules = rule `endBy` spaces <* eof

makeGraph :: [(String, [(String, Int)])] -> DGraph String Int
makeGraph = fromArcsList . foldMap makeArcs
  where makeArcs (src, dests) = map (uncurry (Arc src)) dests

crawl' :: DGraph String Int -> String -> Int
crawl' graph baby = succ $ sum $ map (\(Arc _ d weight) -> weight * crawl graph d) $ outboundingArcs graph baby

crawl :: DGraph String Int -> String -> Int
crawl graph baby = pred $ crawl' graph baby

main :: IO ()
main = do
  let fileName = "in.txt"

  raw <- readFile fileName

  let Right theRules = parse rules fileName raw
      graph = makeGraph theRules

  print $ crawl graph "shiny gold"
