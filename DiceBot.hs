module Main where

import Network
import System.IO
import System.Exit (exitSuccess)
import Text.Printf
import Control.Monad (forever)
import qualified Network.IRC as IRC

import Dice
import DBParser

data DBMsg = DBMsg
  { msgFrom :: IRC.UserName
  , msgCmd  :: DiceBotCmd
  , msgPriv :: Bool
  } deriving (Show, Eq)

data DBCfg = DBCfg
  { cfgServer   :: IRC.ServerName
  , cfgPort     :: PortNumber
  , cfgChannel  :: IRC.Channel
  , cfgNickname :: IRC.UserName
  , cfgHandle   :: Handle
  } deriving Show

-- DiceBot configuration
defaultDBCfg = DBCfg
  { cfgServer   = "irc.freenode.org"
  , cfgPort     = 6667
  , cfgChannel  = "#haskell-ro"
  , cfgNickname = "hsDiceBot"
  , cfgHandle   = error "No default socket available"
  }

hostname    = cfgNickname defaultDBCfg
servername  = "*"
realname    = "A Haskell Dice Bot"

main :: IO ()
main = do
  let server  = cfgServer defaultDBCfg
      port    = cfgPort defaultDBCfg
      nick    = cfgNickname defaultDBCfg
      chan    = cfgChannel defaultDBCfg
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
        let msg = mkDBMsg m
        respond h msg
      -- TODO: use a library pong command
      "PING"    -> do
        let srv = head . IRC.msg_params $ m
            msg = IRC.Message Nothing "PONG" [srv]
        write h msg
      _         -> return ()

respond :: Handle -> DBMsg -> IO ()
respond h m = case msgCmd m of
  Start   -> respStart
  Quit    -> respQuit
  Roll ds -> respRoll ds
  Bad cmd -> respBad cmd
  _       -> return ()
  where
  chan = cfgChannel defaultDBCfg
  toNick = if msgPriv m then msgFrom m else chan
  who = if msgPriv m then "You" else msgFrom m
  respStart = write h $ IRC.privmsg chan "--- Session start ---"
  respQuit = do
    write h $ IRC.privmsg chan "--- Session quit ---"
    write h $ IRC.quit (Just "You and your friends are dead.")
    exitSuccess
  respRoll ds = do
    rs <- rollDice ds
    write h $ IRC.privmsg toNick $ who ++ " " ++
      "rolled " ++ showDice ds ++ ": "
                ++ showResult rs ++ " = " ++ show (sum rs)
  respBad cmd = write h . IRC.privmsg toNick $ who ++ " " ++
      "sent " ++ cmd ++ ": bad command"

mkDBMsg :: IRC.Message -> DBMsg
mkDBMsg m = DBMsg usr cmd prv
  where
  nick = cfgNickname defaultDBCfg
  IRC.NickName usr _ _  = maybe unknown id $ IRC.msg_prefix m
  cmd               = parseCmd . (!! 1) . IRC.msg_params $ m
  prv               = if (head . IRC.msg_params) m == nick then True else False
  unknown = IRC.NickName "(unknown)" Nothing Nothing

-- debugging utilities
dbgOut :: String -> IO ()
dbgOut s = putStrLn $ "out> " ++ s
-- dbgOut s = return ()

dbgIn :: String -> IO ()
dbgIn s = putStrLn $ "in> " ++ s
-- dbgIn s = return ()
