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
  liftIO $ write h $ IRC.nick nick
  liftIO $ write h $ IRC.user nick hostname servername realname
  liftIO $ write h $ IRC.joinChan chan
  listen

listen :: DiceBot ()
listen = do
  (h, n) <- fmap (cfgHandle &&& cfgNickname) ask
  forever $ do
    -- TODO: top-down refactoring
    s <- liftIO $ hGetLine h
    liftIO $ dbgIn s
    --dbgIn $ show $ IRC.decode s
    case IRC.decode s of
      Nothing -> return ()
      Just m  -> case IRC.msg_command m of
        "PRIVMSG" -> do
          let msg = mkDBMsg n m
          respond msg
        -- TODO: use a library pong command
        "PING"    -> do
          let srv = head . IRC.msg_params $ m
              msg = IRC.Message Nothing "PONG" [srv]
          liftIO $ write h msg
        _         -> return ()

respond :: DBMsg -> DiceBot ()
respond m = do
  h <- fmap cfgHandle ask
  case msgCmd m of
    Start   -> respStart h
    Quit    -> respQuit h
    Roll ds -> respRoll ds h
    Bad cmd -> respBad cmd h
    _       -> return ()
  where
  chan = cfgChannel defaultDBCfg
  toNick = if msgPriv m then msgFrom m else chan
  who = if msgPriv m then "You" else msgFrom m

  -- rsponse functions
  respStart h = liftIO $ write h $ IRC.privmsg chan "--- Session start ---"
  respQuit h = do
    liftIO $ write h $ IRC.privmsg chan "--- Session quit ---"
    liftIO $ write h $ IRC.quit (Just "You and your friends are dead.")
    liftIO $ exitSuccess
  respRoll ds h = do
    rs <- liftIO $ rollDice ds
    liftIO . write h . IRC.privmsg toNick $ who ++ " " ++
      "rolled " ++ showDice ds ++ ": "
                ++ showResult rs ++ " = " ++ show (sum rs)
  respBad cmd h = liftIO . write h . IRC.privmsg toNick $ who ++ " " ++
      "sent " ++ cmd ++ ": bad command"

write :: Handle -> IRC.Message -> IO ()
write h m = do
  let raw = IRC.encode m
  hPrintf h "%s\r\n" raw
  dbgOut raw

mkDBMsg :: IRC.UserName -> IRC.Message -> DBMsg
mkDBMsg n m = DBMsg usr cmd prv
  where
  IRC.NickName usr _ _  = maybe unknown id $ IRC.msg_prefix m
  cmd               = parseCmd . (!! 1) . IRC.msg_params $ m
  prv               = if (head . IRC.msg_params) m == n then True else False
  unknown = IRC.NickName "(unknown)" Nothing Nothing

-- debugging utilities
dbgOut :: String -> IO ()
dbgOut s = putStrLn $ "out> " ++ s
-- dbgOut s = return ()

dbgIn :: String -> IO ()
dbgIn s = putStrLn $ "in> " ++ s
-- dbgIn s = return ()
