{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module IIRCC.IRC.Session (
  Session (..),
  EventReport (..),
  Command (..),
  IRCPipes.IrcMessage (..),
  start
) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Catch as C
import Control.Monad.State.Lazy
import Data.Either.Combinators (leftToMaybe)
import Data.Functor.Contravariant
import Data.Maybe (isNothing)
import Data.Text
import GHC.IO.Exception

import qualified Irc.Message as IrcMsg
import Irc.RawIrcMsg (RawIrcMsg)
import qualified Irc.RawIrcMsg as IrcRawMsg
import qualified Irc.Commands as IrcCmd

import Pipes
import Pipes.Prelude as PP
import Pipes.Concurrent as PC
import Pipes.Exhaustion
import Pipes.Safe as PS

import Network.Simple.TCP (Socket, HostName, ServiceName)
import qualified Network.Simple.TCP as TCP
import qualified IIRCC.IRC.Pipes as IRCPipes

data Session =
  Session {
    task :: Async (),
    commandSink :: Output Command
  }

data EventReport =
  Connecting HostName ServiceName |
  Connected |
  FailedToConnect IOError |
  ReceivedMessage IRCPipes.IrcMessage |
  SentMessage RawIrcMsg |
  UnableTo Command Text |
  Disconnected |
  LostConnection (Maybe IOError) |
  Ended
  deriving (Show)

data Command =
  Connect |
  Disconnect |
  Close |
  SendMessage RawIrcMsg
  deriving (Show)

data Message =
  Command Command |
  IrcMessage IRCPipes.IrcMessage |
  MessageReceiverEnded (Maybe IOError)
  deriving (Show)

start :: HostName -> ServiceName -> Output EventReport -> IO Session
start hostName serviceName eventSink = do
  (messageSink, messageSource, sealMailbox) <- spawn' unbounded
  sessionTask <- async $ do
    runEffect $ inexhaustible (fromInput messageSource) >-> sessionPipe (inexhaustible $ toOutput messageSink) >-> toOutput eventSink
    runEffect $ toOutput eventSink <-< yield Ended
    atomically sealMailbox
  return $ Session sessionTask (contramap Command messageSink)

  where
    sessionPipe :: Consumer Message IO () -> Pipe Message EventReport IO ()
    sessionPipe toMailbox = fix $ \sessionPipe' -> await >>= \case
      Command Connect -> do
        yield $ Connecting hostName serviceName
        C.try (TCP.connectSock hostName serviceName) >>= \case
          Left ioe -> do
            yield $ FailedToConnect ioe
            sessionPipe'
          Right (socket, _) ->
            (
              do
                yield Connected
                msgReceiver <- liftIO $ async $ do
                  runEffect $
                    toMailbox <-< (
                      C.try (IRCPipes.receiver 4096 socket) >-> PP.map IrcMessage
                      >>= yield . MessageReceiverEnded . leftToMaybe
                    )
                endReason <- runEffect $ connectedSessionPipe >-> IRCPipes.sender socket
                liftIO $ do
                  TCP.closeSock socket
                  wait msgReceiver
                return endReason
              `C.onException`
              liftIO (TCP.closeSock socket)
            ) >>= \case
              Command Close -> return ()
              Command Disconnect -> do
                yield Disconnected
                sessionPipe'
              MessageReceiverEnded ioe -> do
                yield $ LostConnection ioe
                sessionPipe'
      Command Close -> return ()
      Command Disconnect -> do
        yield $ UnableTo Disconnect "Not connected."
        sessionPipe'
      Command c@(SendMessage _) -> do
        yield $ UnableTo c "Not connected."
        sessionPipe'
      MessageReceiverEnded _ -> sessionPipe' -- End of message receiver from last connection.

    -- Returns event that prompted termination.
    connectedSessionPipe :: Producer RawIrcMsg (Pipe Message EventReport IO) Message
    connectedSessionPipe = awaitMessage >>= \case
      IrcMessage m -> do
        yieldEventReport $ ReceivedMessage m
        connectedSessionPipe
      Command (SendMessage m) -> do
        yieldRawIrcMsg m
        yieldEventReport $ SentMessage m
        connectedSessionPipe
      c@(Command Disconnect) -> return c
      c@(Command Close) -> return c
      c@(MessageReceiverEnded _) -> return c 
      Command Connect -> do
        yieldEventReport $ UnableTo Connect "Already connected."
        connectedSessionPipe

      where
        awaitMessage = lift await
        yieldRawIrcMsg = yield
        yieldEventReport = lift . yield
