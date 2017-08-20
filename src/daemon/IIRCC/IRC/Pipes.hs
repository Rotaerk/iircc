{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module IIRCC.IRC.Pipes (
  IrcMessage (..),
  receiver,
  sender
) where

import Control.Applicative
import Control.Exception
import Control.Monad.Catch as C
import Data.Attoparsec.ByteString as ABS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text

import Irc.Message
import Irc.RawIrcMsg

import Pipes
import Pipes.Core
import Pipes.Exhaustion
import qualified Pipes.Prelude as PP

import IIRCC.TCP.Pipes (Socket)
import qualified IIRCC.TCP.Pipes as TCPPipes

data IrcMessage = ValidIrcMessage IrcMsg | InvalidIrcMessage Text deriving (Show)

receiver :: (MonadCatch io, MonadIO io) => Int -> Socket -> Producer IrcMessage io ()
receiver bytesPerReceive socket = TCPPipes.receiver socket +>> receivedChunksToIrcMessages bytesPerReceive

sender :: MonadIO io => Socket -> Consumer RawIrcMsg io a
sender socket = PP.map renderRawIrcMsg >-> TCPPipes.sender socket

receivedChunksToIrcMessages :: Monad m => Int -> Proxy Int ByteString () IrcMessage m r
receivedChunksToIrcMessages bytesPerChunk = inexhaustible $ receivedChunksToLinesWith parseLine >-> PP.map (textLineToIrcMessage . asUtf8)
  where receivedChunksToLinesWith parseNextLine = request bytesPerChunk >>= linesOfChunkWith parseNextLine >>= receivedChunksToLinesWith

linesOfChunkWith :: Monad m => (ByteString -> Result ByteString) -> ByteString -> Producer' ByteString m (ByteString -> Result ByteString)
linesOfChunkWith parseFirstLine chunk =
  case parseFirstLine chunk of
    Done restOfChunk line -> yield line >> linesOfChunkWith parseLine restOfChunk
    Partial parseLineCont -> return parseLineCont
    Fail restOfInput context errorMessage -> throw . AssertionFailed $ "The line parser failed, but this should've been logically impossible.  The parser accepts all input until it encounters a CR and/or LF, so it should only ever return Done or Partial.\n\nError Message: " ++ errorMessage ++ "\nContext: " ++ show context ++ "\nRest Of Input: " ++ show restOfInput

parseLine :: ByteString -> Result ByteString
parseLine = parse $ ABS.takeTill (`BS.elem` "\CR\LF") <* (pCR <* optional pLF <|> pLF)
  where [pCR, pLF] = ABS.word8 <$> BS.unpack "\CR\LF"

textLineToIrcMessage :: Text -> IrcMessage
textLineToIrcMessage line = maybe (InvalidIrcMessage line) (ValidIrcMessage . cookIrcMsg) (parseRawIrcMsg line)
