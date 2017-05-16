{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.Attoparsec.ByteString as ABS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Tagged
import Pipes
import qualified Pipes.Attoparsec as PA

import Network.IRC.Parser

import qualified TCP
import qualified IRC

-- Run a test IRC daemon with:  ngircd -f ~/ngircd.conf -n

main :: IO ()
main = do
  c <- TCP.connect "localhost" "6667" 4096
  runEffect $ do
    TCP.toConnection c <-< do
      yield $ TCP.Send "NICK rotaerk\r\n"
      yield $ TCP.Send "USER rotaerk 0 * :Matt\r\n"
    for (TCP.fromConnection c >-> tcpToIrc) $ liftIO . print
    TCP.toConnection c <-< yield TCP.Close
  wait $ TCP.connectionTask c

tcpToIrc :: MonadIO m => Pipe TCP.Event IRC.Event m ()
tcpToIrc = processEvents parseMessage
  where
    parseMessage :: ByteString -> Result IRC.Message
    parseMessage = ABS.parse . unTagged $ tpMessage (tpPrefix IRC.PrefixNickName IRC.PrefixServerName) IRC.Message

    processEvents :: MonadIO m => (ByteString -> Result IRC.Message) -> Pipe TCP.Event IRC.Event m ()
    processEvents parseFirstMessage = await >>= \case
      TCP.Received chunk -> parseMessagesFromChunkWith parseFirstMessage chunk >>= mapM_ processEvents
      TCP.Closed -> yield IRC.SessionEnded
      TCP.Disconnected -> yield IRC.Disconnected

    parseMessagesFromChunkWith :: MonadIO m => (ByteString -> Result IRC.Message) -> ByteString -> Pipe TCP.Event IRC.Event m (Maybe (ByteString -> Result IRC.Message))
    parseMessagesFromChunkWith parseFirstMessage chunk =
      case parseFirstMessage chunk of
        Fail restOfInput context errorMessage -> do
          liftIO . putStrLn $ "IRC parse failure: " ++ errorMessage ++ "\nContext: " ++ show context ++ "\nRest of Input:\n" ++ show restOfInput
          return Nothing
        Partial parseMessageCont -> return $ Just parseMessageCont
        Done restOfChunk message -> do
          yield $ IRC.Received message
          parseMessagesFromChunk restOfChunk 

    parseMessagesFromChunk :: MonadIO m => ByteString -> Pipe TCP.Event IRC.Event m (Maybe (ByteString -> Result IRC.Message))
    parseMessagesFromChunk = parseMessagesFromChunkWith parseMessage
