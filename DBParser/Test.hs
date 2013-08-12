{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DBParser.Test where

-- using QuickCheck to test DBParser

import Test.QuickCheck
import Data.List (inits, intersperse)

import Dice
import DBParser

instance Arbitrary String where
  arbitrary = nonBadGen

prop_nonbad :: String -> Bool
prop_nonbad s = case parseCmd s of
  Bad _ -> False
  _     -> True

check :: IO ()
check = do
  let numTests = 1000
      testArgs = stdArgs { maxSuccess = numTests }
  quickCheckWith testArgs prop_nonbad

-----------------------------------------------------------------------------
startGen :: Gen String
startGen = elements . drop 2 $ inits "!start"

quitGen :: Gen String
quitGen = elements . drop 2 $ inits "!quit"

rollPrefixGen :: Gen String
rollPrefixGen = elements . (++ ["!roll"]) . map (++ " ") . drop 2 . init
              $ inits "!roll"

spaces :: Gen String
spaces = listOf $ return ' '

rollConstGen :: Gen String
rollConstGen = do
  NonNegative x <- arbitrary :: Gen (NonNegative Int)
  return $ show x

-- encapsulate die nums and types in separate types
-- XXX: safe for our purpose
newtype DieNum = DieNum Int deriving Num
newtype DieType = DieType Int deriving Num

instance Arbitrary DieNum where
  arbitrary  = do
    Positive x <- arbitrary :: Gen (Positive Int)
    return . DieNum $ x `mod` 20 + 1

instance Arbitrary DieType where
  arbitrary = do
    Positive x <- arbitrary :: Gen (Positive Int)
    return . DieType $ x `mod` 100 + 1

-- TODO: refactor
rollDieGen' :: Gen String
rollDieGen' = oneof [defNT, defN, defT, nodef]
  where
  defNT = return "d"
  defN = do
    DieType t <- arbitrary :: Gen DieType
    ss <- spaces
    return $ "d" ++ ss ++ show t
  defT = do
    DieNum n <- arbitrary :: Gen DieNum
    ss <- spaces
    return $ show n ++ ss ++ "d"
  nodef = do
    DieType t <- arbitrary :: Gen DieType
    DieNum n <- arbitrary :: Gen DieNum
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
