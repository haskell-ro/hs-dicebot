module Types where

data DiceBotCmd =
    Start
  | Quit
  | Roll { cmdDice :: [Die] }
  | Unknown
  deriving (Show, Eq)

data Die =
    Const { dieConst :: Int }
  | Die { dieDie :: Int }
  deriving (Show, Eq)
