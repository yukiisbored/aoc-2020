{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Control.Monad.State
    ( StateT(StateT), modify, MonadState(get), State, execState )
import Control.Lens ( (^.), over, makeLenses )
import Data.Maybe (catMaybes)

type Bound = (Int, Int)
type Rule = (String, Bound, Bound)
type Ticket = [Int]
type Note = ([Rule], Ticket, [Ticket])

data FinderState = FinderState { _confirmed   :: M.Map Int String
                               , _unconfirmed :: M.Map Int (S.Set String) }
                   deriving (Show)
$(makeLenses ''FinderState)

newtype Finder a = Finder { unFinder :: State FinderState a }
  deriving (Functor, Applicative, Monad, MonadState FinderState)

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

followWho :: [Rule] -> Int -> S.Set String
followWho rs x = S.fromList $ map fst $ filter snd res
  where res = map (\r@(name,_,_) -> (name, follow r x)) rs

possibleRules :: [Rule] -> [Int] -> S.Set String
possibleRules rs xs = L.foldl1' S.intersection (map (followWho rs) xs)

followAny :: [Rule] -> Int -> Bool
followAny rs x = or $ follow <$> rs <*> pure x

isValidTicket :: [Rule] -> Ticket -> Bool
isValidTicket rs = all (followAny rs)

findConfirmed :: M.Map Int (S.Set String) -> M.Map Int String
findConfirmed = M.map S.findMin . M.filter ((== 1) . S.size)

initFinder :: M.Map Int (S.Set String) -> FinderState
initFinder u = FinderState { _confirmed = M.empty
                           , _unconfirmed = u }

-- TODO: Stop using a State Monad :')

strikeConfirmed :: Finder ()
strikeConfirmed = do
  st <- get
  let deleter s = L.foldl' (\x p -> p x) s (S.delete <$> M.elems (st^.confirmed))
  modify $ over unconfirmed (M.map deleter)

find :: Finder ()
find = do
  st <- get

  let confirmed' = findConfirmed $ st^.unconfirmed

  if M.null confirmed' then return ()
  else do
    modify $ over confirmed (M.union confirmed')
    strikeConfirmed
    find


main :: IO ()
main = do
  let fileName = "in.txt"

  raw <- BS.readFile fileName

  let Right (rs, t, ts) = parse note fileName raw

      ticket = M.fromList $ zip [0..] t

      rows        = L.transpose $ filter (isValidTicket rs) ts
      unconfirmed = M.fromList $ zip [0..] $ map (possibleRules rs) rows

      finder'    = execState (unFinder find) (initFinder unconfirmed)
      fieldNames = finder'^.confirmed

      departureFields = M.keys $ M.filter (L.isPrefixOf "departure") fieldNames

  print $ product $ catMaybes $ flip M.lookup ticket <$> departureFields
