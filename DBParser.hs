{-# LANGUAGE NoMonomorphismRestriction #-}
module DBParser (parseCmd) where

import Text.Parsec

import Dice

parseCmd :: String -> DiceBotCmd
parseCmd s = case parse cmd "(unknown)" s of
  Right c -> c
  _       -> None

cmd = do
  bang
  try (start <|> quit <|> roll) <|> fmap Bad getInput

bang  = char '!'
start = char 's' >> mapM_ (optional . char) "tart" >> eof >> return Start
quit = char 'q' >> mapM_ (optional . char) "uit" >> eof >> return Quit

roll = do
  char 'r' >> mapM_ (optional . char) "oll"
  spaces
  ds <- sumDice
  eof
  return $ case checkDice ds of
    Left err  -> Bad err
    Right ds  -> Roll ds

sumDice = do
  d <- die
  ds <- many $ spaces >> char '+' >> spaces >> die
  return (d : ds)

die = try dieVal <|> constVal
constVal = digit `sepEndBy1` spaces >>= return . Right . Const . read
dieVal = do
  dn <- number <|> return 1
  char 'd'
  spaces
  dt <- number <|> return 6
  return . checkDie $ Die dn dt
  where
  number = fmap read $ digit `sepEndBy1` spaces

-- semantic hack: sanity checking for dice
checkDie :: Die -> Either String Die
checkDie d@(Die n t)
  | n == 0 || t == 0 || n > 20 || t > 100 = Left "invalid roll"
  | otherwise                             = Right d

checkDice :: [Either String Die] -> Either String [Die]
checkDice eds = case filter isError eds of
  []            -> Right . map right $ eds
  Left err : _  -> Left err
  where
  isError (Left _)  = True
  isError _         = False
  right (Right r) = r
  right _         = error "Either: Left encountered"
