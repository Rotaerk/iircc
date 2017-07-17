{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module IIRCC.TCP (
  RecvChunkSize,
  Command (..),
  Connection (..),
  IIRCC.TCP.connect,
  IIRCC.TCP.send,
  IIRCC.TCP.close,

  HostName,
  ServiceName
) where

import Control.Concurrent.Async
import qualified Control.Exception as E
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
import qualified Pipes.Safe

import Network.Simple.TCP as T

type RecvChunkSize = Int

data Command = Send ByteString | Close deriving (Show)

data Connection m =
  Connection {
    connectionTask :: Async Bool, -- Result indicates whether the connection terminated cleanly.
    fromConnection :: Producer ByteString m (),
    toConnection :: Consumer Command m ()
  }

connect :: MonadIO m => HostName -> ServiceName -> RecvChunkSize -> IO (Connection m)
connect hostName serviceName bytesPerRecv =
  connectSock hostName serviceName >>= \(socket, _) ->
    do
      (inboxOutput, inboxInput, sealInbox) <- PC.spawn' unbounded
      (outboxOutput, outboxInput, sealOutbox) <- PC.spawn' unbounded
      connection <-
        async $
        do
          en <- async $ run $ eventNotifier socket >-> (toOutput inboxOutput $> shouldn'tExhaust)
          cd <- async $ run $ commandDispatcher socket <-< fromInput outboxInput
          wasClosed <- wait en -- Event notifier ends when socket disconnected or closed
          atomically $ PC.send outboxOutput Close
          wait cd -- Command dispatcher ends after dispatching Close command
          return wasClosed
        `finally`
        do
          closeSock socket
          atomically $ sealInbox >> sealOutbox
      return $ Connection connection (fromInput inboxInput) (toOutput outboxOutput)
    `onException`
    closeSock socket

  where
    run = runEffect

    shouldn'tExhaust :: a
    shouldn'tExhaust = E.throw . AssertionFailed $ "The mailbox should not exhaust before the other end of the pipe returns."

    eventNotifier :: Socket -> Producer ByteString IO Bool
    eventNotifier socket = yieldRest `catchBadFileDescriptor` \_ -> return False
      where
        yieldRest =
          T.recv socket bytesPerRecv >>= \case
            Just bs -> yield bs >> yieldRest
            Nothing -> return True

        catchBadFileDescriptor = catchIf $ ("Bad file descriptor" ==) . ioe_description

    commandDispatcher :: Socket -> Consumer Command IO ()
    commandDispatcher socket = awaitRest
      where
        awaitRest =
          await >>= \case
            Send bs -> T.send socket bs >> awaitRest
            Close -> closeSock socket

send :: Monad m => ByteString -> Producer Command m ()
send = yield . Send

close :: Monad m => Producer Command m ()
close = yield Close
