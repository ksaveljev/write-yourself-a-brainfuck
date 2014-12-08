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
        bfToChar GoRight     = '>'
        bfToChar GoLeft      = '<'
        bfToChar Increment   = '+'
        bfToChar Decrement   = '-'
        bfToChar Print       = '.'
        bfToChar Read        = ','
        bfToChar LoopL       = '['
        bfToChar LoopR       = ']'
        bfToChar (Comment c) = c


parseBrainfuck :: String -> Either String BrainfuckSource
parseBrainfuck = checkSyntax . BrainfuckSource . mapMaybe charToBF
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

checkSyntax :: BrainfuckSource -> Either String BrainfuckSource
checkSyntax bfSource@(BrainfuckSource commands) = verify commands 0 []
  where
    verify :: [BrainfuckCommand] -> Int -> [Int] -> Either String BrainfuckSource
    verify (LoopL:xs) pos l = verify xs (pos+1) (pos:l)
    verify (LoopR:xs) pos l | not (null l) = verify xs (pos+1) (tail l)
    verify (LoopR:_) pos _ = Left $ "Mismatched closing parenthesis at position " ++ show pos
    verify (_:xs) pos l = verify xs (pos+1) l
    verify [] _ [] = Right bfSource
    verify [] _ (pos:_) = Left $ "Mismatched opening parenthesis at position " ++ show pos 
