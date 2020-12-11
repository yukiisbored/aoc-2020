#!/usr/bin/env runhaskell

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.Map.Strict as M
import Control.Monad.State (execState, modify, when, get, MonadState, State)

type Line = M.Map Int Char
type BoardState = M.Map Int (M.Map Int Char)

newtype Board a = Board { unBoard :: State BoardState a }
  deriving (Functor, Applicative, Monad, MonadState BoardState)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

bLookup :: BoardState -> Int -> Int -> Maybe Char
bLookup b x y = M.lookup y b >>= M.lookup x

bShow :: BoardState -> String
bShow b = unlines $ M.elems <$> M.elems b

present :: BoardState -> Int -> Int -> Bool
present b x y =
  case bLookup b x y of
    Just '#' -> True
    _        -> False

neighbors :: BoardState -> Int -> Int -> Int
neighbors b x y = length $ filter id
                  [ present b (x + x') (y + y') | x' <- [-1..1]
                                                , y' <- [-1..1]
                                                , (x', y') /= (0, 0) ]

tickCell :: BoardState -> Int -> Int -> Char
tickCell b x y
  | c == '.' = '.'
  | c == 'L' = if nb == 0 then '#'
               else 'L'
  | c == '#' = if nb >= 4 then 'L'
               else '#'
  where nb     = neighbors b x y
        Just c = bLookup   b x y

tickLine :: BoardState -> Int -> M.Map Int Char
tickLine b y = M.mapWithKey (\x _ -> tickCell b x y) (b M.! y)

tick :: BoardState -> BoardState
tick b = M.mapWithKey (\y _ -> tickLine b y) b

parseMap :: String -> BoardState
parseMap = M.fromList . map transformLine . enumerate . lines
  where transformLine (i, xs) = (i, M.fromList $ enumerate xs)

calculateBoard :: Board ()
calculateBoard = do
  board  <- get
  modify tick
  board' <- get
  when (board /= board') calculateBoard

main :: IO ()
main = do
  raw <- readFile "in.txt"

  let m  = parseMap raw
      m' = execState (unBoard calculateBoard) m

  print $ length $ filter (== '#') $ bShow m'
