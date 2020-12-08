#!/usr/bin/env runhaskell

{-# LANGUAGE TemplateHaskell #-}

import Control.Lens (element, set,  (^?), (^.), over, makeLenses, Ixed(ix) )
import Text.Parsec.String (Parser)
import Text.Parsec
    (string, choice,  digit, oneOf, spaces, endBy, eof, many1, parse )
import Data.Maybe (isNothing)
import qualified Data.Vector as V
import qualified Data.Set as S
import Control.Monad.State.Strict
    ( modify, execState, MonadState(get), State )

type Op = Int

data Instruction = Nop Op
                    | Jmp Op
                    | Acc Op
                    deriving (Show, Eq)

type Program = V.Vector Instruction

data CPUStatus = Run
               | CyclicHalt
               | Halt
               deriving (Show, Eq)

data CPUState = CPUState { _pc      :: Int
                         , _acc     :: Int
                         , _visited :: S.Set Int
                         , _status   :: CPUStatus
                         , _program :: Program }
              deriving (Show)
$(makeLenses ''CPUState)

type CPU a = State CPUState a

operand :: Parser Int
operand = do
  sign <- oneOf "+-"
  n <- many1 digit

  let num' = read n :: Int
      num = if sign == '-' then -num'
            else num'

  return num

instruction :: Parser Instruction
instruction = do
  ins <- choice (string <$> ["nop", "jmp", "acc"]) <* spaces
  op <- operand

  case ins of
    "nop" -> return $ Nop op
    "jmp" -> return $ Jmp op
    "acc" -> return $ Acc op

assembly :: Parser Program
assembly = V.fromList <$> instruction `endBy` spaces <* eof

execute' :: CPU ()
execute' = do
  cpu <- get

  let ins = cpu^.program^?ix (cpu^.pc)
      cyclicCheck = (cpu^.pc) `S.member` (cpu^.visited)
      halt = cyclicCheck || isNothing ins

  if halt
    then do
      modify $ set status (if cyclicCheck then CyclicHalt else Halt)
      return ()
    else do
      modify (over visited (S.insert (cpu^.pc)))
      case ins of
        Just (Jmp op) -> modify (over pc (+ op))
        Just (Acc op) -> modify (over pc (+ 1) . over acc (+ op))
        Just (Nop _)  -> modify (over pc (+ 1))
      execute'

execute :: Program -> CPUState
execute prog = execState execute' (CPUState 0 0 S.empty Run prog)

patch :: Instruction -> Instruction
patch (Jmp op) = Nop op
patch (Nop op) = Jmp op
patch a = a

bruteforce :: Program -> CPUState
bruteforce prog = head $ [cpu' | i <- [0..(length prog - 1)]
                               , let prog' = over (element i) patch prog
                               , let cpu' = execute prog'
                               , cpu'^.status == Halt]

main :: IO ()
main = do
  let fileName = "in.txt"

  raw <- readFile fileName

  let program' = parse assembly fileName raw

  case program' of
    Left err      -> print err
    Right program -> print $ bruteforce program^.acc
