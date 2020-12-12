#!/usr/bin/env runhaskell

import Text.Parsec.ByteString (Parser)
import Text.Parsec (parse, eof, endBy, spaces, (<|>), try, string, choice, digit, many1, oneOf)
import qualified Data.ByteString as BS

data Cardinal = North
              | East
              | South
              | West
              deriving (Show, Eq, Enum)

type Unit = Int
type Location = (Unit, Unit)
type Ship = (Location, Cardinal)

data Direction = L
               | R
               deriving (Show, Eq)

data Command = Move Cardinal Unit
             | Forward Unit
             | Rotate Direction Int
             deriving (Show, Eq)

int :: Parser Int
int = do
  n <- many1 digit
  let num = read n :: Int
  return num

unitCommand :: Parser Command
unitCommand = do
  cmd <- oneOf "NSEWF"
  num <- int

  case cmd of
    'N' -> return $ Move North num
    'E' -> return $ Move East num
    'S' -> return $ Move South num
    'W' -> return $ Move West num
    'F' -> return $ Forward num

rotateCommand :: Parser Command
rotateCommand = do
  dir <- oneOf "LR"
  deg <- choice [ string "0"
                , string "90"
                , string "180"
                , string "270" ]

  let mul = case deg of
        "0"   -> 0
        "90"  -> 1
        "180" -> 2
        "270" -> 3

  case dir of
    'L' -> return $ Rotate L mul
    'R' -> return $ Rotate R mul

command :: Parser Command
command = try unitCommand <|> rotateCommand

commands :: Parser [Command]
commands = spaces *> command `endBy` spaces <* eof

move :: Ship -> Command -> Ship
move ((x, y), c) (Move North n) = ((x    , y + n), c)
move ((x, y), c) (Move East  n) = ((x + n, y    ), c)
move ((x, y), c) (Move South n) = ((x    , y - n), c)
move ((x, y), c) (Move West  n) = ((x - n, y    ), c)

move (xy, c) (Rotate R n) = (xy, c')
  where c' :: Cardinal
        c' = toEnum $ (fromEnum c + n) `mod` 4

move s@((_, _), c) (Forward  n) = move s $ Move   c      n
move s             (Rotate L n) = move s $ Rotate R (4 - n)

manhattan :: Ship -> Int
manhattan ((x, y), _) = abs x + abs y

follow :: Ship -> [Command] -> Ship
follow = foldl move

solve :: [Command] -> Int
solve cmds = manhattan $ follow init cmds
  where init = ((0, 0), East)

main :: IO ()
main = do
  let fileName = "in.txt"

  raw <- BS.readFile fileName

  let Right cmds = parse commands fileName raw

  print $ solve cmds
