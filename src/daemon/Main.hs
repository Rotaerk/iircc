{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Network.IIRCC

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (IOException)
import Control.Monad
import Control.Monad.Catch as C
import Data.ByteString as BS (ByteString)
import Data.Typeable
import Network.Simple.TCP as TCP
import Network.Socket (close)
import Pipes
import Pipes.Concurrent as PC
import Pipes.Core
import qualified Pipes.Prelude as PP
import Pipes.Safe as PS (SafeT, runSafeT)
import System.IO.Error
import GHC.IO.Exception

-- Run a test IRC daemon with:  ngircd -n

main :: IO ()
main = do
  (connection, connInput, connOutput) <- runConnection "localhost" "6667" 4096
  atomically $ do
    PC.send connOutput $ Send "NICK rotaerk\r\n"
    PC.send connOutput $ Send "USER rotaerk 0 * :Matt\r\n"
  runEffect $ for (fromInput connInput) $ liftIO . print
  wait connection

data ConnectionCommand = Send ByteString | Close deriving (Show)
data ConnectionEvent = Received ByteString | Closed | Disconnected deriving (Show)

runConnection :: HostName -> ServiceName -> Int -> IO (Async (), Input ConnectionEvent, Output ConnectionCommand)
runConnection hostName serviceName bytesPerRecv =
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
      return (connection, inboxInput, outboxOutput)
    `onException`
    closeSock socket

  where
    run = runEffect

    eventNotifier :: Socket -> Producer ConnectionEvent IO ()
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

    commandDispatcher :: Socket -> Consumer ConnectionCommand IO ()
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
