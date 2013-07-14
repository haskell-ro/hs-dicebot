module Dice where

data DiceBotCmd =
    Start
  | Quit
  | Roll { cmdDice :: [Die] }
  | Unknown
  deriving (Show, Eq)

data Die =
    Const { dieConst :: Int }
  | Die { dieNum :: Int, dieType :: Int }
  deriving (Show, Eq)

showDice :: [Die] -> String
showDice [] = ""
showDice (d : ds)
  | null ds = showDie d
  | otherwise = showDie d ++ " + " ++ showDice ds
  where
  showDie (Const n) = show n
  showDie (Die n t) = show n ++ "d" ++ show t
