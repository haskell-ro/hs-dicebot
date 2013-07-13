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
  start <|> quit

bang  = char '!'
start = char 's' >> mapM_ (optional . char) "tart" >> eof >> return Start
quit = char 'q' >> mapM_ (optional . char) "uit" >> eof >> return Quit
