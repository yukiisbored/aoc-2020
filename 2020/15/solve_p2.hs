#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Bifunctor (second)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Either (rights)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Control.Monad.State.Strict (when, execState, get, modify, MonadState, State)

data GameState = GameState { previous :: M.Map Int [Int]
                           , numbers  :: [Int]
                           , turn     :: Int }
               deriving (Show)

newtype Game a = Game { unGame :: State GameState a}
  deriving (Functor, Applicative, Monad, MonadState GameState)

initState :: [Int] -> GameState
initState xs = GameState { previous = M.fromList turns
                         , numbers  = ns
                         , turn     = t }
  where ns    = reverse xs
        turns = zipWith (curry $ second (:[])) xs [1..]
        t     = length xs

newNumber :: GameState -> (Int, [Int])
newNumber state = (n, turn':prevs)
  where n          = 0
        turn'      = succ $ turn state
        Just prevs = M.lookup n $ previous state

oldNumber :: GameState -> (Int, [Int])
oldNumber state = (n, turn':prevs)
  where c:_          = numbers state
        Just (x:y:_) = M.lookup c $ previous state
        n            = x - y
        turn'        = succ $ turn state
        prevs        = fromMaybe [] $ M.lookup n $ previous state

nextNumber :: GameState -> GameState
nextNumber state = GameState { previous = M.insert n ps' $ previous state
                             , numbers  = n : numbers state
                             , turn     = turn' }
  where c:_   = numbers state
        turn' = succ $ turn state
        Just ps = M.lookup c $ previous state
        (n, ps') = case ps of
                     _:_:_ -> oldNumber state
                     _:_   -> newNumber state

playUntil :: Int -> Game ()
playUntil x = do
  modify nextNumber
  s <- get
  when (turn s < x) $ playUntil x

parse :: T.Text -> [Int]
parse = map fst . rights . map TR.decimal . T.splitOn "," . head . T.lines

main :: IO ()
main = do
  raw <- TIO.readFile "in.txt"

  let xs = parse raw
      s  = execState (unGame (playUntil 30000000)) (initState xs)
      y:_  = numbers s

  print y
