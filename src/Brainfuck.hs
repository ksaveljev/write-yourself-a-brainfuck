module Brainfuck () where

data BrainfuckCommand = GoRight      -- >
                      | GoLeft       -- <
                      | Increment    -- +
                      | Decrement    -- -
                      | Print        -- .
                      | Read         -- ,
                      | LoopL        -- [
                      | LoopR        -- ]
                      | Comment Char -- anything else

type BrainfuckSource = [BrainfuckCommand]

parseBrainfuck :: String -> BrainfuckSource
parseBrainfuck = map charToBF
  where
    charToBF '>' = GoRight
    charToBF '<' = GoLeft
    charToBF '+' = Increment
    charToBF '-' = Decrement
    charToBF '.' = Print
    charToBF ',' = Read
    charToBF '[' = LoopL
    charToBF ']' = LoopR
    charToBF c   = Comment c
