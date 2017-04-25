{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.IIRCC

import Control.Concurrent (threadDelay)
import Control.Exception (IOException)
import Control.Monad
import Data.Typeable
import Network.IRC
import Network.Simple.TCP
import Pipes
import Pipes.Core
import Pipes.Safe
import qualified Pipes.Network.TCP.Safe as PTCP
import System.IO.Error

main :: IO ()
main = runSafeT . runEffect $ for (ircSession 10 servers) $ liftIO . print
  where
    servers =
      cycle [
        ("aoeuaoeuaoeusntg.com", "999"),
        ("blarghskxl.org", "666"),
        ("youtube.com", "80")
      ]

ircSession :: Int -> [(HostName, ServiceName)] -> Producer IrcEvent (SafeT IO) ()
ircSession reconnectDelay = ircSession'
  where
    ircSession' [] = return ()
    ircSession' ((hostName, serviceName):otherServers) = do
      endEvent <- ircConnection hostName serviceName
      unless (endEvent == ClosedConnection) $ do
        liftIO . threadDelay $ 1000000 * reconnectDelay
        ircSession' otherServers

ircConnection :: HostName -> ServiceName -> Producer IrcEvent (SafeT IO) IrcEvent
ircConnection hostName serviceName = do
  endEvent <-
    handleIf
      (\ex -> isDoesNotExistError ex && ioeGetLocation ex == "getAddrInfo")
      (const . return $ FailedToConnect hostName serviceName) $
    PTCP.connect hostName serviceName $
    \(socket, socketAddr) -> do
      yield $ Connected hostName serviceName
      liftIO . threadDelay $ 5000000
      return ClosedConnection
  yield endEvent
  return endEvent
