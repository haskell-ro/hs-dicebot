{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Network
import System.IO
import System.Exit (exitSuccess)
import Text.Printf
import Control.Monad (forever, when)
import Control.Monad.Reader
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
  , cfgDebug    :: Bool
  } deriving Show

defaultDBCfg = DBCfg
  { cfgServer   = "irc.freenode.org"
  , cfgPort     = 6667
  , cfgChannel  = "#haskell-ro"
  , cfgNickname = "hsDiceBot"
  , cfgHandle   = error "No default socket available"
  , cfgDebug    = False
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
  s <- dbGetLine
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
  case msgCmd m of
    Start   -> respStart
    Quit    -> respQuit
    Roll ds -> respRoll ds
    Bad cmd -> respBad cmd
    _       -> return ()
  where
  toNick = if msgPriv m then writeNick $ msgFrom m else writeChan
  who = if msgPriv m then "You" else msgFrom m

  -- response functions
  respStart = writeChan "--- Session start ---"
  respQuit = do
    writeChan "--- Session quit ---"
    write $ IRC.quit (Just "You and your friends are dead.")
    dbexit
  respRoll ds = do
    rs <- roll ds
    toNick $ who ++ " " ++
      "rolled " ++ showDice ds ++ ": "
                ++ showResult rs ++ " = " ++ show (sum rs)
  respBad cmd = toNick $ who ++ " " ++
      "sent " ++ cmd ++ ": bad command"

-- auxiliary functions
writeChan :: String -> DiceBot ()
writeChan s = fmap cfgChannel ask >>= write . flip IRC.privmsg s

writeNick :: IRC.UserName -> String -> DiceBot ()
writeNick n s = write . IRC.privmsg n $ s

write :: IRC.Message -> DiceBot ()
write m = do
  let raw = IRC.encode m
  h <- fmap cfgHandle ask
  liftIO $ hPrintf h "%s\r\n" raw
  dbgOut raw

roll :: [Die] -> DiceBot [Int]
roll = liftIO . rollDice

dbGetLine :: DiceBot String
dbGetLine = fmap cfgHandle ask >>= liftIO . hGetLine

dbexit :: DiceBot ()
dbexit = liftIO $ exitSuccess

mkDBMsg :: IRC.UserName -> IRC.Message -> DBMsg
mkDBMsg n m = DBMsg usr cmd prv
  where
  IRC.NickName usr _ _  = maybe unknown id $ IRC.msg_prefix m
  cmd               = parseCmd . (!! 1) . IRC.msg_params $ m
  prv               = if (head . IRC.msg_params) m == n then True else False
  unknown = IRC.NickName "(unknown)" Nothing Nothing

-- debugging utilities
dbgOut :: String -> DiceBot ()
dbgOut s = do
  debug <- fmap cfgDebug ask
  when debug $ liftIO . putStrLn $ "out> " ++ s

dbgIn :: String -> DiceBot ()
dbgIn s = do
  debug <- fmap cfgDebug ask
  when debug $ liftIO . putStrLn $ "in> " ++ s
