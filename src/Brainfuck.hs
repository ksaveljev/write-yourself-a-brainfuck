module Brainfuck () where

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

type BrainfuckSource = [BrainfuckCommand]

parseBrainfuck :: String -> BrainfuckSource
parseBrainfuck = mapMaybe charToBF
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
