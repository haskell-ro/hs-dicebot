{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Network
import System.IO
import System.Exit (exitSuccess)
import Text.Printf
import Control.Monad (forever)
import Control.Monad.Reader
import Control.Arrow ((&&&))
import qualified Network.IRC as IRC

import Dice
import DBParser

-- DiceBot configuration
data DBCfg = DBCfg
  { cfgServer   :: IRC.ServerName
  , cfgPort     :: PortNumber
  , cfgChannel  :: IRC.Channel
  , cfgNickname :: IRC.UserName
  , cfgHandle   :: Handle
  } deriving Show

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

-- useful structures for the DiceBot app
data DBMsg = DBMsg
  { msgFrom :: IRC.UserName
  , msgCmd  :: DiceBotCmd
  , msgPriv :: Bool
  } deriving (Show, Eq)

-- XXX: is it safe to use generalized newtype deriving?
newtype DiceBot a = DiceBot { runDiceBot :: ReaderT DBCfg IO a }
  deriving (Functor, Monad, MonadIO, MonadReader DBCfg)

evalDiceBot :: DBCfg -> DiceBot a -> IO a
evalDiceBot cfg = flip runReaderT cfg . runDiceBot

-- TODO: get config from command line arguments
main :: IO ()
main = do
  let server  = cfgServer defaultDBCfg
      port    = cfgPort defaultDBCfg
  h <- connectTo server (PortNumber port)
  hSetBuffering h NoBuffering

  -- set up handle and run
  let cfg = defaultDBCfg { cfgHandle = h }
  evalDiceBot cfg dbmain

dbmain :: DiceBot ()
dbmain = do
  cfg <- ask
  let nick    = cfgNickname cfg
      chan    = cfgChannel cfg
      h       = cfgHandle cfg
  write $ IRC.nick nick
  write $ IRC.user nick hostname servername realname
  write $ IRC.joinChan chan
  listen

listen :: DiceBot ()
listen = forever $ do
  -- TODO: wrap this in another function
  s <- fmap cfgHandle ask >>= liftIO . hGetLine
  dbgIn s
  --dbgIn $ show $ IRC.decode s
  case IRC.decode s of
    Nothing -> return ()
    Just m  -> case IRC.msg_command m of
      "PRIVMSG" -> do
        n <- fmap cfgNickname ask
        let msg = mkDBMsg n m
        respond msg
      -- TODO: use a library pong command
      "PING"    -> do
        let srv = head . IRC.msg_params $ m
            msg = IRC.Message Nothing "PONG" [srv]
        write msg
      _         -> return ()

respond :: DBMsg -> DiceBot ()
respond m = do
  h <- fmap cfgHandle ask
  case msgCmd m of
    Start   -> respStart
    Quit    -> respQuit
    Roll ds -> respRoll ds
    Bad cmd -> respBad cmd
    _       -> return ()
  where
  chan = cfgChannel defaultDBCfg
  toNick = if msgPriv m then msgFrom m else chan
  who = if msgPriv m then "You" else msgFrom m

  -- rsponse functions
  respStart = write $ IRC.privmsg chan "--- Session start ---"
  respQuit = do
    write $ IRC.privmsg chan "--- Session quit ---"
    write $ IRC.quit (Just "You and your friends are dead.")
    -- TODO: wrap this in another function
    liftIO $ exitSuccess
  respRoll ds = do
    -- TODO: wrap this in another function
    rs <- liftIO $ rollDice ds
    write . IRC.privmsg toNick $ who ++ " " ++
      "rolled " ++ showDice ds ++ ": "
                ++ showResult rs ++ " = " ++ show (sum rs)
  respBad cmd = write . IRC.privmsg toNick $ who ++ " " ++
      "sent " ++ cmd ++ ": bad command"

write :: IRC.Message -> DiceBot ()
write m = do
  let raw = IRC.encode m
  h <- fmap cfgHandle ask
  liftIO $ hPrintf h "%s\r\n" raw
  dbgOut raw

mkDBMsg :: IRC.UserName -> IRC.Message -> DBMsg
mkDBMsg n m = DBMsg usr cmd prv
  where
  IRC.NickName usr _ _  = maybe unknown id $ IRC.msg_prefix m
  cmd               = parseCmd . (!! 1) . IRC.msg_params $ m
  prv               = if (head . IRC.msg_params) m == n then True else False
  unknown = IRC.NickName "(unknown)" Nothing Nothing

-- debugging utilities
dbgOut :: String -> DiceBot ()
dbgOut s = liftIO . putStrLn $ "out> " ++ s
-- dbgOut s = return ()

dbgIn :: String -> DiceBot ()
dbgIn s = liftIO . putStrLn $ "in> " ++ s
-- dbgIn s = return ()
