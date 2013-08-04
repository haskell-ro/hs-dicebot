module Main where

import Network
import System.IO
import System.Exit (exitSuccess)
import Text.Printf
import Control.Monad (forever)
import qualified Network.IRC as IRC

import Dice
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
    Just m  -> case IRC.msg_command m of
      "PRIVMSG" -> do
        let msg = (!! 1) . IRC.msg_params $ m
            c   = parseCmd msg
        respond h c
      -- TODO: use a library pong command
      "PING"    -> do
        let srv = head . IRC.msg_params $ m
            msg = IRC.Message Nothing "PONG" [srv]
        write h msg
      _         -> return ()

respond :: Handle -> DiceBotCmd -> IO ()
respond h c = case c of
  Start   -> write h $ IRC.privmsg chan "--- Session start ---"
  Quit    -> respQuit
  Roll ds -> respRoll ds
  Bad cmd -> respBad cmd
  _       -> return ()
  where
  respQuit = do
    write h $ IRC.privmsg chan "--- Session quit ---"
    write h $ IRC.quit (Just "You and your friends are dead.")
    exitSuccess
  respRoll ds = do
    rs <- rollDice ds
    write h $ IRC.privmsg chan $
      "Roll " ++ showDice ds ++ ": "
              ++ showResult rs ++ " = " ++ show (sum rs)
  respBad cmd = write h . IRC.privmsg chan $ cmd ++ ": bad command"

-- debugging utilities
dbgOut :: String -> IO ()
dbgOut s = putStrLn $ "out> " ++ s
-- dbgOut s = return ()

dbgIn :: String -> IO ()
dbgIn s = putStrLn $ "in> " ++ s
-- dbgIn s = return ()
