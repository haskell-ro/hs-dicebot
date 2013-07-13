module Main where

import Network
import System.IO
import Text.Printf
import Control.Monad (forever)
import qualified Network.IRC as IRC

import Types
import DBParser

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
  --dbgIn $ show $ IRC.decode s
  case IRC.decode s of
    Nothing -> return ()
    Just m  -> if IRC.msg_command m /= "PRIVMSG" then return ()
        else do
        let msg = (!! 1) . IRC.msg_params $ m
            c = parseCmd msg
        respond h c

respond :: Handle -> DiceBotCmd -> IO ()
respond h c = case c of
  Start   -> write h $ IRC.privmsg chan "--- Session start ---"
  Quit    -> write h $ IRC.privmsg chan "--- Session quit ---"
  Roll _  -> write h $ IRC.privmsg chan "--- Unimplemented ---"
  _       -> return ()

-- debugging utilities
dbgOut :: String -> IO ()
dbgOut s = putStrLn $ "out> " ++ s
-- dbgOut s = return ()

dbgIn :: String -> IO ()
dbgIn s = putStrLn $ "in> " ++ s
-- dbgIn s = return ()
