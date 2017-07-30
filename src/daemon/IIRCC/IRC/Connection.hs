{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module IIRCC.IRC.Connection (
  Message (..),
  receiver,
  sender
) where

import Control.Applicative
import Control.Exception
import Data.Attoparsec.ByteString as ABS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text

import Irc.Message
import Irc.RawIrcMsg

import Pipes
import Pipes.Core
import qualified Pipes.Prelude as PP

import IIRCC.TCP (Socket)
import qualified IIRCC.TCP as TCP

data Message = ValidMessage IrcMsg | InvalidMessage Text deriving (Show)

receiver :: Int -> Socket -> Producer Message IO (Maybe IOError)
receiver bytesPerReceive socket = TCP.receiver socket +>> (receivedChunksToMessages bytesPerReceive *> return Nothing)

sender :: Socket -> Consumer RawIrcMsg IO ()
sender socket = PP.map renderRawIrcMsg >-> TCP.sender socket

receivedChunksToMessages :: Monad m => Int -> Proxy Int ByteString () Message m ()
receivedChunksToMessages bytesPerChunk = receivedChunksToLinesWith parseLine >-> PP.map (textLineToMessage . asUtf8)
  where receivedChunksToLinesWith parseNextLine = request bytesPerChunk >>= linesOfChunkWith parseNextLine >>= receivedChunksToLinesWith

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

textLineToMessage :: Text -> Message
textLineToMessage line = maybe (InvalidMessage line) (ValidMessage . cookIrcMsg) (parseRawIrcMsg line)
