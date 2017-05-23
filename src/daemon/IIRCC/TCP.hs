{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module IIRCC.TCP (
  RecvChunkSize,
  Command (..),
  Event (..),
  Connection (..),
  IIRCC.TCP.connect,
  IIRCC.TCP.send,
  IIRCC.TCP.close,
  eventPipe,

  HostName,
  ServiceName
) where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.ByteString
import Data.Functor
import System.IO.Error
import GHC.IO.Exception

import Pipes
import Pipes.Prelude as PP
import Pipes.Concurrent as PC
import Pipes.Partial
import qualified Pipes.Safe

import Network.Simple.TCP as T

type RecvChunkSize = Int

data Command = Send ByteString | Close deriving (Show)
data Event = Received ByteString | Closed | Disconnected deriving (Show)

data Connection m =
  Connection {
    connectionTask :: Async (),
    fromConnection :: Producer Event m (),
    toConnection :: Consumer Command m ()
  }

connect :: MonadIO m => HostName -> ServiceName -> RecvChunkSize -> IO (Connection m)
connect hostName serviceName bytesPerRecv =
  connectSock hostName serviceName >>= \(socket, _) ->
    do
      (inboxOutput, inboxInput, sealInbox) <- spawn' unbounded
      (outboxOutput, outboxInput, sealOutbox) <- spawn' unbounded
      connection <-
        async $
        do
          en <- async $ run $ eventNotifier socket >-> toOutput inboxOutput
          cd <- async $ run $ commandDispatcher socket <-< fromInput outboxInput
          wait en -- Event notifier ends when socket disconnected or closed
          atomically $ PC.send outboxOutput Close
          wait cd -- Command dispatcher ends after dispatching Close command
        `finally`
        do
          closeSock socket
          atomically $ sealInbox >> sealOutbox
      return $ Connection connection (fromInput inboxInput) (toOutput outboxOutput)
    `onException`
    closeSock socket

  where
    run = runEffect

    eventNotifier :: Socket -> Producer Event IO ()
    eventNotifier socket =
      let
        yieldRest = do
          maybeBytes <- T.recv socket bytesPerRecv
          case maybeBytes of
            Just bs -> do
              yield $ Received bs
              yieldRest
            Nothing -> yield Disconnected
      in yieldRest
      `catchBadFileDescriptor`
      \_ -> yield Closed

      where
        catchBadFileDescriptor = catchIf $ \ex ->
          ioe_description ex == "Bad file descriptor"

    commandDispatcher :: Socket -> Consumer Command IO ()
    commandDispatcher socket =
      let
        awaitRest = do
          command <- await
          case command of
            Send bs -> do
              T.send socket bs
              awaitRest
            Close -> closeSock socket
      in awaitRest

send :: Monad m => ByteString -> Producer Command m ()
send = yield . Send

close :: Monad m => Producer Command m ()
close = yield Close

eventPipe
  :: Monad m
  => Pipe ByteString o m r -- Used to yield o-values from received chunks
  -> o -- o-value to yield for Closed events
  -> o -- o-value to yield for Disconnected events
  -> Pipe Event o m r
eventPipe receivedChunkPipe closed disconnected =
  toPartial >->
  partialPipe matchReceivedEvent receivedChunkPipe >->
  partialMap ((closed <$) . guard . isClosedEvent) >->
  partialMap ((disconnected <$) . guard . isDisconnectedEvent) >->
  complete
  
  where
    matchReceivedEvent (Received bs) = Just bs
    matchReceivedEvent _ = Nothing

    isClosedEvent Closed = True
    isClosedEvent _ = False

    isDisconnectedEvent Disconnected = True
    isDisconnectedEvent _ = False
