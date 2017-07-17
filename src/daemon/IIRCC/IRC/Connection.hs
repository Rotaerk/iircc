{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module IIRCC.IRC.Connection (
  Command (..),
  Event (..),
  Connection (..),
  connect,
  send,
  close,
  eventPipe,

  HostName,
  Port
) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Attoparsec.ByteString as ABS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty
import Data.Text
import Data.These

import Irc.Message
import Irc.RawIrcMsg

import Pipes
import qualified Pipes.Prelude as PP
import Pipes.Branching

import IIRCC.TCP (ServiceName, HostName)
import qualified IIRCC.TCP as TCP

data Command = Send RawIrcMsg | Close deriving (Show)
data Event = Received IrcMsg | InvalidMsg Text deriving (Show)

data Connection m =
  Connection {
    connectionTask :: Async Bool, -- Result indicates whether the connection terminated cleanly.
    fromConnection :: Producer Event m (),
    toConnection :: Consumer Command m ()
  }

type Port = ServiceName

connect :: MonadIO m => HostName -> Port -> IO (Connection m)
connect hostName port = do
  tcpConnection <- TCP.connect hostName port 4096
  return $
    Connection {
      connectionTask = TCP.connectionTask tcpConnection,
      fromConnection = TCP.fromConnection tcpConnection >-> chunksToLines >-> PP.map (textLineToEvent . asUtf8),
      toConnection = ircCommandsToTcpCommands >-> TCP.toConnection tcpConnection
    }

send :: Monad m => RawIrcMsg -> Producer Command m ()
send = yield . Send

close :: Monad m => Producer Command m ()
close = yield Close

ircCommandsToTcpCommands :: Monad m => Pipe Command TCP.Command m ()
ircCommandsToTcpCommands = commandPipe (TCP.Send . renderRawIrcMsg) TCP.Close

textLineToEvent :: Text -> Event
textLineToEvent line = maybe (InvalidMsg line) (Received . cookIrcMsg) (parseRawIrcMsg line)

chunksToLines :: Monad m => Pipe ByteString ByteString m ()
chunksToLines = chunksToLinesWith parseLine
  where chunksToLinesWith parseNextLine = await >>= linesOfChunkWith parseNextLine >>= chunksToLinesWith

linesOfChunkWith :: Monad m => (ByteString -> Result ByteString) -> ByteString -> Producer' ByteString m (ByteString -> Result ByteString)
linesOfChunkWith parseFirstLine chunk =
  case parseFirstLine chunk of
    Done restOfChunk line -> yield line >> linesOfChunkWith parseLine restOfChunk
    Partial parseLineCont -> return parseLineCont
    Fail restOfInput context errorMessage -> throw . AssertionFailed $ "The line parser failed, but this should've been logically impossible.  The parser accepts all input until it encounters a CR and/or LF, so it should only ever return Done or Partial.\n\nError Message: " ++ errorMessage ++ "\nContext: " ++ show context ++ "\nRest Of Input: " ++ show restOfInput

parseLine :: ByteString -> Result ByteString
parseLine = parse pLine
  where
    pLine :: Parser ByteString
    pLine = ABS.takeTill (`BS.elem` "\CR\LF") <* (pCR <* optional pLF <|> pLF)

    [pCR, pLF] = ABS.word8 <$> BS.unpack "\CR\LF"

commandPipe
  :: Monad m
  => (RawIrcMsg -> o) -- Map the IRC messages from Send commands to o-values
  -> o -- o-value to yield for Close commands
  -> Pipe Command o m ()
commandPipe fromSendMsg close =
  remap ((fromSendMsg <$>) . matchSendCommand) $
  PP.map (\Close -> close)

  where
    matchSendCommand (Send rawIrcMsg) = Just rawIrcMsg
    matchSendCommand _ = Nothing

eventPipe
  :: Monad m
  => (IrcMsg -> o) -- Map the IRC messages from Received events to o-values
  -> (Text -> o) -- Map the text from InvalidMsg events to o-values
  -> Pipe Event o m ()
eventPipe fromReceivedMsg fromInvalidMsgText =
  remap ((fromReceivedMsg <$>) . matchReceivedEvent) $
  PP.map (\(InvalidMsg text) -> fromInvalidMsgText text)

  where
    matchReceivedEvent (Received msg) = Just msg
    matchReceivedEvent _ = Nothing
