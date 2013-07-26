module Dice where

import Control.Monad (liftM)
import Control.Monad.Random
import Data.List (intercalate)

data DiceBotCmd =
    Start
  | Quit
  | Roll { cmdDice :: [Die] }
  | None
  | Bad { cmdBad :: String }
  deriving (Show, Eq)

data Die =
    Const { dieConst :: Int }
  | Die { dieNum :: Int, dieType :: Int }
  deriving (Show, Eq)

showDice :: [Die] -> String
showDice = intercalate " + " . map showDie
  where showDie (Const n) = show n
        showDie (Die n t) = show n ++ "d" ++ show t

showResult :: [Int] -> String
showResult = intercalate " + " . map show

randomFace :: RandomGen g => Int -> Rand g Int
randomFace t = getRandomR (1, t)

rollDie :: RandomGen g => Die -> Rand g [Int]
rollDie (Const n) = return [n]
rollDie (Die n t) = sequence . replicate n $ randomFace t

rollDice :: [Die] -> IO [Int]
rollDice = evalRandIO . liftM concat . mapM rollDie
