module Brainfuck where

import Data.Maybe (mapMaybe)

data BrainfuckCommand = GoRight      -- >
                      | GoLeft       -- <
                      | Increment    -- +
                      | Decrement    -- -
                      | Print        -- .
                      | Read         -- ,
                      | LoopL        -- [
                      | LoopR        -- ]
                      | Comment Char -- anything else

newtype BrainfuckSource = BrainfuckSource [BrainfuckCommand]

instance Show BrainfuckSource where
    show (BrainfuckSource commands) = map bfToChar commands
      where
        bfToChar GoRight = '>'
        bfToChar GoLeft = '<'
        bfToChar Increment = '+'
        bfToChar Decrement = '-'
        bfToChar Print = '.'
        bfToChar Read = ','
        bfToChar LoopL = '['
        bfToChar LoopR = ']'
        bfToChar (Comment c) = c


parseBrainfuck :: String -> BrainfuckSource
parseBrainfuck = BrainfuckSource . mapMaybe charToBF
  where
    charToBF '>' = Just GoRight
    charToBF '<' = Just GoLeft
    charToBF '+' = Just Increment
    charToBF '-' = Just Decrement
    charToBF '.' = Just Print
    charToBF ',' = Just Read
    charToBF '[' = Just LoopL
    charToBF ']' = Just LoopR
    charToBF _   = Nothing
