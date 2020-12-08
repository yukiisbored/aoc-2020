#!/usr/bin/env runhaskell

{-# LANGUAGE TemplateHaskell #-}

import Control.Lens (element, set,  (^?), (^.), over, makeLenses, Ixed(ix) )
import Text.Parsec.String (Parser)
import Text.Parsec
    ( digit, letter, oneOf, spaces, endBy, eof, many1, parse )
import Data.Maybe (isJust)

type Op = Int

data CPUInstruction = Nop Op
                    | Jmp Op
                    | Acc Op
                    deriving (Show, Eq)

type Program = [CPUInstruction]

data State = Run
           | CyclicHalt
           | Halt
           deriving (Show, Eq)

data CPU = CPU { _pc      :: Int
               , _acc     :: Int
               , _visited :: [Int]
               , _state   :: State }
         deriving (Show)

$(makeLenses ''CPU)


operand :: Parser Int
operand = do
  sign <- oneOf "+-"
  n <- many1 digit

  let num' = read n :: Int
      num = if sign == '-' then -num'
            else num'

  return num

instruction :: Parser CPUInstruction
instruction = do
  ins <- many1 letter <* spaces
  op <- operand

  case ins of
    "nop" -> return $ Nop op
    "jmp" -> return $ Jmp op
    "acc" -> return $ Acc op
    _     -> error "Unknown instruction"

assembly :: Parser [CPUInstruction]
assembly = instruction `endBy` spaces <* eof

initCPU :: CPU
initCPU = CPU { _pc      = 0
              , _acc     = 0
              , _visited = []
              , _state   = Run }

execute :: Program -> CPU -> CPU
execute []   cpu = cpu
execute prog cpu = if continue then (execute prog . execute' ins)
                                    (over visited (cpu^.pc:) cpu)
                   else set state (if cyclicCheck then CyclicHalt else Halt) cpu

  where ins = prog^?ix (cpu^.pc)

        cyclicCheck = cpu^.pc `elem` (cpu^.visited)
        continue = not cyclicCheck && isJust ins

        execute' :: Maybe CPUInstruction -> CPU -> CPU
        execute' (Just (Jmp op)) = over pc (+ op)
        execute' (Just (Acc op)) = over pc (+ 1) . over acc (+ op)
        execute' (Just (Nop _))  = over pc (+ 1)
        execute' Nothing         = id

patch :: CPUInstruction -> CPUInstruction
patch (Jmp op) = Nop op
patch (Nop op) = Jmp op
patch a = a

bruteforce :: Program -> CPU
bruteforce program =
  head . filter (\x -> x^.state == Halt)
  $ execute <$> [over (element i) patch program | i <- [0..(length program - 1)]] <*> pure initCPU

main :: IO ()
main = do
  let fileName = "in.txt"

  raw <- readFile fileName

  let Right program = parse assembly fileName raw
      cpu = bruteforce program

  print (cpu^.acc)
