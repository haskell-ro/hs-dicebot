{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module DBParser.Test where

-- using QuickCheck to test DBParser

import Test.QuickCheck
import Data.List (inits)

import DBParser

instance Arbitrary String where
  arbitrary = nonBadGen

-----------------------------------------------------------------------------
startGen :: Gen String
startGen = elements . drop 2 $ inits "!start"

quitGen :: Gen String
quitGen = elements . drop 2 $ inits "!quit"

rollPrefixGen :: Gen String
rollPrefixGen = elements . drop 2 $ inits "!roll"

spaces :: Gen String
spaces = listOf $ return ' '

rollConstGen :: Gen String
rollConstGen = do
  NonNegative x <- arbitrary :: Gen (NonNegative Int)
  return $ show x

-- TODO: refactor
rollDieGen' :: Gen String
rollDieGen' = oneof [defNT, defN, defT, nodef]
  where
  defNT = return "d"
  defN = do
    Positive t <- arbitrary :: Gen (Positive Int)
    ss <- spaces
    return $ "d" ++ ss ++ show t
  defT = do
    Positive n <- arbitrary :: Gen (Positive Int)
    ss <- spaces
    return $ show n ++ ss ++ "d"
  nodef = do
    Positive t <- arbitrary :: Gen (Positive Int)
    Positive n <- arbitrary :: Gen (Positive Int)
    ss <- spaces
    ss' <- spaces
    return $ show n ++ ss ++ "d" ++ ss' ++ show t

rollDieGen :: Gen String
rollDieGen = oneof [rollConstGen, rollDieGen']

rollDieSumGen :: Gen String
rollDieSumGen = do
  ss <- spaces
  ss' <- spaces
  v <- rollDieGen
  return $ ss ++ "+" ++ ss' ++  v

rollGen :: Gen String
rollGen = do
  p <- rollPrefixGen
  ss <- spaces
  v <- rollDieGen
  vs <- listOf rollDieSumGen
  return $ p ++ ss ++ v ++ concat vs

-- TODO: randomly generate strings without "!" prefix
noneGen :: Gen String
noneGen = return "bla"

nonBadGen :: Gen String
nonBadGen = oneof [startGen, quitGen, rollGen, noneGen]
