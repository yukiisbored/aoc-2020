import qualified Data.ByteString as BS
import Text.Parsec.ByteString (Parser)
import Text.Parsec
    ( alphaNum,
      char,
      digit,
      spaces,
      string,
      many1,
      manyTill,
      sepBy1,
      (<|>),
      parse,
      try )
type Bound = (Int, Int)
type Rule = (String, Bound, Bound)
type Ticket = [Int]
type Note = ([Rule], Ticket, [Ticket])

int :: Parser Int
int = do
  n <- many1 digit
  let num = read n :: Int
  return num

bound :: Parser Bound
bound = do
  l <- int <* char '-'
  h <- int
  return (l, h)

rule :: Parser Rule
rule = do
  name <- many1 (alphaNum <|> char ' ') <* string ": "
  b1   <- bound <* string " or "
  b2   <- bound <* spaces

  return (name, b1, b2)

ticket :: Parser Ticket
ticket = int `sepBy1` char ',' <* spaces

note :: Parser Note
note = do
  rules <- manyTill rule (try (string "your ticket:" <* spaces))
  mine <- ticket
  tickets <- string "nearby tickets:" *> spaces *> many1 ticket
  return (rules, mine, tickets)

within :: Bound -> Int -> Bool
within (low, high) x = x >= low && x <= high

follow :: Rule -> Int -> Bool
follow (_, b1, b2) x = within b1 x || within b2 x

followAny :: [Rule] -> Int -> Bool
followAny rs x = or $ follow <$> rs <*> pure x

findBadNumbers :: [Rule] -> Ticket -> [Int]
findBadNumbers rs = filter (not . followAny rs)

errorRate :: [Rule] -> [Ticket] -> Int
errorRate rs = sum . concatMap (findBadNumbers rs)

main :: IO ()
main = do
  let fileName = "in.txt"

  raw <- BS.readFile fileName

  let Right (rs, _, ts) = parse note fileName raw

  print $ errorRate rs ts
