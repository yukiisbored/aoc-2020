#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (second)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Either (rights)
import qualified Data.Map.Lazy as M
import Data.Maybe (fromMaybe)

data GameState = GameState { previous :: M.Map Int [Int]
                           , numbers  :: [Int]
                           , turn     :: Int }
               deriving (Show)

initState :: [Int] -> GameState
initState xs = GameState { previous = M.fromList turns
                         , numbers  = ns
                         , turn     = t }
  where ns    = reverse xs
        turns = zipWith (curry $ second (:[])) xs [1..]
        t     = length xs

next :: GameState -> GameState
next state = GameState { previous = M.insert n ps' $ previous state
                       , numbers  = n : numbers state
                       , turn     = turn' }
  where c:_     = numbers state
        turn'   = succ $ turn state
        Just ps = M.lookup c $ previous state
        n       = case ps of
                    x:y:_ -> x - y
                    _:_   -> 0
        ps'     = turn' : fromMaybe [] (M.lookup n $ previous state)

playUntil :: Int -> GameState -> GameState
playUntil times init = foldl (flip ($)) init (replicate times' next)
  where times' = times - turn init

parse :: T.Text -> [Int]
parse = map fst . rights . map TR.decimal . T.splitOn "," . head . T.lines

main :: IO ()
main = do
  raw <- TIO.readFile "in.txt"

  let xs   = parse raw
      s    = playUntil 30000000 (initState xs)
      y:_  = numbers s

  print y
