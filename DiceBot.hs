module Main where

import Network
import qualified Network.IRC as IRC
import System.IO
import Text.Printf
import Control.Monad (forever)

server      = "irc.freenode.org"
port        = 6667
chan        = "#haskell-ro"
nick        = "hsDiceBot"
hostname    = nick
servername  = "*"
realname    = "A Haskell Dice Bot"

main :: IO ()
main = do
  h <- connectTo server (PortNumber port)
  hSetBuffering h NoBuffering
  write h $ IRC.nick nick
  write h $ IRC.user nick hostname servername realname
  write h $ IRC.joinChan chan
  listen h

write :: Handle -> IRC.Message -> IO ()
write h m = do
  let raw = IRC.encode m
  hPrintf h "%s\r\n" raw
  dbgOut raw

listen :: Handle -> IO ()
listen h = forever $ do
  s <- hGetLine h
  dbgIn s

-- debugging utilities
dbgOut :: String -> IO ()
dbgOut s = putStrLn $ "out> " ++ s

dbgIn :: String -> IO ()
dbgIn s = putStrLn $ "in> " ++ s
