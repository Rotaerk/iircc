{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Connection (
  RecvChunkSize,
  Command (..),
  Event (..),
  Connection (..),
  open 
) where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.ByteString
import System.IO.Error
import GHC.IO.Exception

import Pipes
import Pipes.Concurrent as PC
import qualified Pipes.Safe

import Network.Simple.TCP as TCP

type RecvChunkSize = Int

data Command = Send ByteString | Close deriving (Show)
data Event = Received ByteString | Closed | Disconnected deriving (Show)

data Connection m =
  Connection {
    task :: Async (),
    from :: Producer Event m (),
    to :: Consumer Command m ()
  }

open :: MonadIO m => HostName -> ServiceName -> RecvChunkSize -> IO (Connection m)
open hostName serviceName bytesPerRecv =
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
          maybeBytes <- TCP.recv socket bytesPerRecv
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
              TCP.send socket bs
              awaitRest
            Close -> closeSock socket
      in awaitRest
