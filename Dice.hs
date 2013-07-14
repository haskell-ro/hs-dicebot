module Dice where

import Control.Monad (liftM)
import Control.Monad.Random

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

showResult :: [Int] -> String
showResult [] = ""
showResult (n : ns)
  | null ns = show n
  | otherwise = show n ++ " + " ++ showResult ns

randomFace :: RandomGen g => Int -> Rand g Int
randomFace t = getRandomR (1, t)

rollDie :: RandomGen g => Die -> Rand g [Int]
rollDie (Const n) = return [n]
rollDie (Die n t) = sequence . replicate n $ randomFace t

rollDice :: RandomGen g => [Die] -> Rand g [Int]
rollDice = liftM concat . mapM rollDie
