{-# LANGUAGE NoMonomorphismRestriction #-}
module DBParser where

import Text.Parsec

import Types

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
  d <- die
  eof
  return $ Roll [d]

die = constVal <|> dieVal
constVal = alphaNum `sepEndBy1` spaces >>= return . Const . read
dieVal = return $ Die 2 42
