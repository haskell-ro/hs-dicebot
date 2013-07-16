{-# LANGUAGE NoMonomorphismRestriction #-}
module DBParser (parseCmd) where

import Text.Parsec

import Dice

parseCmd :: String -> DiceBotCmd
parseCmd s = case parse cmd "(unknown)" s of
  Right c -> c
  _       -> Unknown

cmd = do
  bang
  start <|> quit <|> roll

bang  = char '!'
start = char 's' >> mapM_ (optional . char) "tart" >> eof >> return Start
quit = char 'q' >> mapM_ (optional . char) "uit" >> eof >> return Quit

roll = do
  char 'r' >> mapM_ (optional . char) "oll"
  spaces
  ds <- sumDice
  eof
  return $ Roll ds

sumDice = do
  d <- die
  ds <- many $ spaces >> char '+' >> spaces >> die
  return (d : ds)

die = try dieVal <|> constVal
constVal = digit `sepEndBy1` spaces >>= return . Const . read
dieVal = numDies <|> oneDie
  where
  oneDie = char 'd' >> digit `sepEndBy1` spaces >>= return . Die 1 . read
  numDies = do
    sn <- many1 digit
    char 'd'
    st <- digit `sepEndBy1` spaces
    return $ Die (read sn) (read st)
