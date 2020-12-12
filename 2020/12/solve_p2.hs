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
type Waypoint = Location
type Ship = (Waypoint, Location)

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

start :: Ship
start = ((10, 1), (0, 0))

rotate :: Location -> Int -> Location
rotate l      0 = l
rotate (x, y) 1 = ( y, -x)
rotate (x, y) 2 = (-x, -y)
rotate (x, y) 3 = (-y,  x)
rotate l      m = rotate l $ m `mod` 4

move :: Ship -> Command -> Ship
move ((x', y'), l) (Move North n) = ((x'    , y' + n), l)
move ((x', y'), l) (Move East  n) = ((x' + n, y'    ), l)
move ((x', y'), l) (Move South n) = ((x'    , y' - n), l)
move ((x', y'), l) (Move West  n) = ((x' - n, y'    ), l)

move ((x', y'), (x, y)) (Forward n) = ((x', y'), (x + (x' * n), y + (y' * n)))

move (w, l) (Rotate R m) = (rotate w m, l)

move s (Rotate L m) = move s $ Rotate R (-m)

manhattan :: Ship -> Int
manhattan (_, (x, y)) = abs x + abs y

follow :: Ship -> [Command] -> Ship
follow = foldl move

solve :: [Command] -> Int
solve cmds = manhattan $ follow start cmds

main :: IO ()
main = do
  let fileName = "in.txt"
  raw <- BS.readFile fileName
  let Right cmds = parse commands fileName raw
  print $ solve cmds
