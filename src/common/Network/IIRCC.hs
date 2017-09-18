{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.IIRCC where

import Codec.CBOR.Read
import Codec.CBOR.Write
import Codec.Serialise
import Codec.Serialise.Encoding
import Codec.Serialise.Decoding
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Dependent.Sum
import Data.Either.Combinators
import Data.Functor.Identity
import Data.Monoid
import Data.Text
import Data.Word
import GHC.Generics

type ChannelName = Text
type SessionName = Text
type QuitMessage = Text
type UninterpretedMessage = Text
type ErrorMessage = Text
type HostName = Text
type PortNumber = Word16

data ClientCommand =
  StartSession SessionName HostName PortNumber |
  ToSession SessionName SessionCommand |
  EndClient
  deriving (Show, Generic)

instance Serialise ClientCommand

data SessionCommand =
  Connect |
  Join ChannelName |
  --ToChannel ChannelName ChannelCommand |
  SendQuit QuitMessage |
  Disconnect |
  EndSession
  deriving (Show, Generic)

instance Serialise SessionCommand

--data ChannelCommand =
--  SendMessage Text |
--  Part

data ClientEventTag f a where
  SessionStarted :: ClientEventTag f SessionName
  FromSession :: f (SessionName, f (SessionEventTag f a)) -> ClientEventTag f a
  ClientUnableTo :: ClientEventTag f (ClientCommand, ErrorMessage)
  ClientEnding :: ClientEventTag f ()

type ClientEventFilter = ClientEventTag Maybe
type ClientEventHeader = ClientEventTag Identity
type ClientEvent = DSum ClientEventHeader Identity

data SessionEventTag (f :: * -> *) a where
  Connecting :: SessionEventTag f (HostName, PortNumber)
  Connected :: SessionEventTag f ()
  FailedToConnect :: SessionEventTag f ErrorMessage
  --FromChannel :: ChannelName -> ChannelEventTag a -> SessionEventTag a
  ReceivedUninterpretedMessage :: SessionEventTag f UninterpretedMessage
  SessionUnableTo :: SessionEventTag f (SessionCommand, ErrorMessage)
  SentQuit :: SessionEventTag f QuitMessage
  EndedConnection :: SessionEventTag f ()
  LostConnection :: SessionEventTag f (Maybe ErrorMessage)
  EndedSession :: SessionEventTag f ()

type SessionEventFilter = SessionEventTag Maybe
type SessionEventHeader = SessionEventTag Identity
type SessionEvent = DSum SessionEventHeader Identity

--data ChannelEventTag a where
--  ReceivedMessage :: ChannelEventTag Text
--  SentMessage :: ChannelEventTag Text

{-
data ChannelEventTag a where
-}

-- encodeSessionEventFilter :: SessionEventFilter -> Encoding
-- encodeSessionEventFilter = \case
--   (Connecting :=> m) -> encodeOptionalWithTagNum 0 m
--   (Connected :=> _) -> encodeEmptyWithTagNum 1
--   (FailedToConnect :=> m) -> encodeOptionalWithTagNum 2 m
--   (ReceivedUninterpretedMessage :=> m) -> encodeOptionalWithTagNum 3 m
--   (SessionUnableTo :=> m) -> encodeOptionalWithTagNum 4 m
--   (SentQuit :=> m) -> encodeOptionalWithTagNum 5 m
--   (EndedConnection :=> _) -> encodeEmptyWithTagNum 6
--   (LostConnection :=> m) -> encodeOptionalWithTagNum 7 m
--   (EndedSession :=> _) -> encodeEmptyWithTagNum 8

--   where
--     encodeWithTagNum n e = encodeTag n <> e <> encodeNull
--     encodeOptionalWithTagNum n = encodeWithTagNum n . maybe mempty encode
--     encodeEmptyWithTagNum n = encodeWithTagNum n mempty

testFilter :: ClientEventFilter (HostName, PortNumber)
testFilter = FromSession $ Just $ ("someSession", Just Connecting)

testFilter2 :: ClientEventFilter a
testFilter2 = FromSession $ Just $ ("someSession", Nothing)

testHeader :: ClientEventHeader (HostName, PortNumber)
testHeader = FromSession $ Identity $ ("someSession", Identity Connecting)

-- The encoding of a filter needs to be a prefix of a valid header encoding.
encodeClientEventFilter :: ClientEventFilter a -> Encoding
encodeClientEventFilter = \case
  SessionStarted -> encodeTag 0 <> encodeNull
  FromSession Nothing -> encodeTag 1
  FromSession (Just (sessionName, Nothing)) -> encodeTag 1 <> encodeString sessionName <> encodeNull
  FromSession (Just (sessionName, Just sessionEventFilter)) -> encodeTag 1 <> encodeString sessionName <> encodeNull <> encodeSessionEventFilter sessionEventFilter
  ClientUnableTo -> encodeTag 2 <> encodeNull
  ClientEnding -> encodeTag 3 <> encodeNull

encodeSessionEventFilter :: SessionEventFilter a -> Encoding
encodeSessionEventFilter = \case
  Connecting -> encodeTag 0 <> encodeNull
  Connected -> encodeTag 1 <> encodeNull
  FailedToConnect -> encodeTag 2 <> encodeNull
  --FromChannel -> encodeTag 3
  ReceivedUninterpretedMessage -> encodeTag 4 <> encodeNull
  SessionUnableTo -> encodeTag 5 <> encodeNull
  SentQuit -> encodeTag 6 <> encodeNull
  EndedConnection -> encodeTag 7 <> encodeNull
  LostConnection -> encodeTag 8 <> encodeNull
  EndedSession -> encodeTag 9 <> encodeNull

encodeClientEvent :: ClientEvent -> (Encoding, Encoding)
encodeClientEvent = \case
  SessionStarted :=> Identity v -> (encodeTag 0 <> encodeNull, encode v)
  FromSession (Identity (sessionName, Identity sessionEventHeader)) :=> Identity v ->
    mapFst (mappend $ encodeTag 1 <> encodeString sessionName <> encodeNull) $ encodeSessionEvent (sessionEventHeader ==> v)
  ClientUnableTo :=> Identity v -> (encodeTag 2 <> encodeNull, encode v)
  ClientEnding :=> Identity v -> (encodeTag 3 <> encodeNull, encode v)

  where
    mapFst f (a, b) = (f a, b)

decodeClientEvent :: Decoder s (Decoder s ClientEvent)
decodeClientEvent = decodeTag >>= \case
  0 -> decodeNull >> (return $ (SessionStarted ==>) <$> decode)
  1 -> do
    sessionName <- decodeString <* decodeNull
    decodeSessionEvent <&.&> \(sessionEventHeader :=> v) -> FromSession (Identity (sessionName, Identity sessionEventHeader)) :=> v
  2 -> decodeNull >> (return $ (ClientUnableTo ==>) <$> decode)
  3 -> decodeNull >> (return $ (ClientEnding ==>) <$> decode)
  _ -> undefined

  where
    (<$.$>) = (<$>) . (<$>)
    (<&.&>) = flip (<$.$>)

encodeSessionEvent :: SessionEvent -> (Encoding, Encoding)
encodeSessionEvent = \case
  Connecting :=> Identity v -> (encodeTag 0 <> encodeNull, encode v)
  Connected :=> Identity v -> (encodeTag 1 <> encodeNull, encode v)
  FailedToConnect :=> Identity v -> (encodeTag 2 <> encodeNull, encode v)
  ReceivedUninterpretedMessage :=> Identity v -> (encodeTag 3 <> encodeNull, encode v)
  SessionUnableTo :=> Identity v -> (encodeTag 4 <> encodeNull, encode v)
  SentQuit :=> Identity v -> (encodeTag 5 <> encodeNull, encode v)
  EndedConnection :=> Identity v -> (encodeTag 6 <> encodeNull, encode v)
  LostConnection :=> Identity v -> (encodeTag 7 <> encodeNull, encode v)
  EndedSession :=> Identity v -> (encodeTag 8 <> encodeNull, encode v)

decodeSessionEvent :: Decoder s (Decoder s SessionEvent)
decodeSessionEvent = decodeTag >>= \case
  0 -> decodeNull >> (return $ (Connecting ==>) <$> decode)
  1 -> decodeNull >> (return $ (Connected ==>) <$> decode)
  2 -> decodeNull >> (return $ (FailedToConnect ==>) <$> decode)
  3 -> decodeNull >> (return $ (ReceivedUninterpretedMessage ==>) <$> decode)
  4 -> decodeNull >> (return $ (SessionUnableTo ==>) <$> decode)
  5 -> decodeNull >> (return $ (SentQuit ==>) <$> decode)
  6 -> decodeNull >> (return $ (EndedConnection ==>) <$> decode)
  7 -> decodeNull >> (return $ (LostConnection ==>) <$> decode)
  8 -> decodeNull >> (return $ (EndedConnection ==>) <$> decode)
  _ -> undefined

{-
encodeSessionEventTag :: SessionEventTag a -> Encoding
encodeSessionEventTag = \case
  Connecting -> "Connecting."
  Connected -> "Connected."
  FailedToConnect -> "FailedToConnect."
  ReceivedUninterpretedMessage -> "ReceivedUninterpretedMessage."
  SessionUnableTo -> "SessionUnableTo."
  SentQuit -> "SentQuit."
  EndedConnection -> "EndedConnection."
  LostConnection -> "LostConnection."
  EndedSession -> "EndedSession."

encodeSessionEventData :: SessionEvent -> ByteString
encodeSessionEventData = \case
  (Connecting :=> Identity v) -> encodeBS v
  (Connected :=> Identity v) -> encodeBS v
  (FailedToConnect :=> Identity v) -> encodeBS v
  (ReceivedUninterpretedMessage :=> Identity v) -> encodeBS v
  (SessionUnableTo :=> Identity v) -> encodeBS v
  (SentQuit :=> Identity v) -> encodeBS v
  (EndedConnection :=> Identity v) -> encodeBS v
  (LostConnection :=> Identity v) -> encodeBS v
  (EndedSession :=> Identity v) -> encodeBS v

  where
    encodeBS :: Serialise a => a -> ByteString
    encodeBS = toLazyByteString . encode

decodeSessionEvent :: ByteString -> ByteString -> Either String SessionEvent
decodeSessionEvent = \case
  "Connecting." -> decodeAndTagWith Connecting
  "Connected." -> decodeAndTagWith Connected
  "FailedToConnect." -> decodeAndTagWith FailedToConnect
  "ReceivedUninterpretedMessage." -> decodeAndTagWith ReceivedUninterpretedMessage
  "SessionUnableTo." -> decodeAndTagWith SessionUnableTo
  "SentQuit." -> decodeAndTagWith SentQuit
  "EndedConnection." -> decodeAndTagWith EndedConnection
  "LostConnection." -> decodeAndTagWith LostConnection
  "EndedSession." -> decodeAndTagWith EndedSession
  tagBS -> const $ Left $ "Unexpected tag: " ++ BS8.unpack tagBS

  where
    decodeAndTagWith :: Serialise a => SessionEventTag a -> ByteString -> Either String SessionEvent
    decodeAndTagWith t = mapBoth show ((t ==>) . snd) . deserialiseFromBytes decode
-}
{-
encodeClientEvent :: ClientEvent -> (ByteString, ByteString)
encodeClientEvent = \case
  (SessionStarted :=> Identity v) -> ("SessionStarted.", encode v)
  (FromSession sessionName sessionEventTag :=> Identity v) ->
    let (sessionEventTagBS, sessionEventValueBS) = encodeSessionEvent (sessionEventTag ==> v)
    (concat ["FromSession:", ".", encode v)
  (ClientUnableTo :=> Identity v) -> ("ClientUnableTo.", encode v)
  (ClientEnding :=> Identity v) -> ("ClientEnding.", encode v)
-}

{-
encodeClientEventTag :: ClientEventTag a -> ByteString
encodeClientEventTag = \case
  SessionStarted -> "SessionStarted."
  FromSession sessionName sessionEventTag -> BS.concat ["FromSession:", encode sessionName, ".", encodeSessionEventTag sessionEventTag]
  ClientUnableTo -> "ClientUnableTo."
  ClientEnding -> "ClientEnding."
-}

{-
encodeClientEvent :: ClientEvent -> Encoding
encodeClientEvent = \case
  (SessionStarted :=> Identity v) -> encodeAndTagWith "SessionStarted." v
  (FromSession sessionName sessionEventTag :=> Identity v) ->
    BS.concat ["FromSession:", encode sessionName, ".", encodeSessionEvent (sessionEventTag ==> v)]
  (ClientUnableTo :=> Identity v) -> encodeAndTagWith "ClientUnableTo." v
  (ClientEnding :=> Identity v) -> encodeAndTagWith "ClientEnding." v
  
  where
    encodeAndTagWith :: Serialise a => ByteString -> a -> ByteString
    encodeAndTagWith t = BS.append t . encode

encodeSessionEventTag :: SessionEventTag a -> Encoding
encodeSessionEventTag = \case
  Connecting -> "Connecting."
  Connected -> "Connected."
  FailedToConnect -> "FailedToConnect."
  ReceivedUninterpretedMessage -> "ReceivedUninterpretedMessage."
  SessionUnableTo -> "SessionUnableTo."
  SentQuit -> "SentQuit."
  EndedConnection -> "EndedConnection."
  LostConnection -> "LostConnection."
  EndedSession -> "EndedSession."

encodeSessionEventData :: SessionEvent -> ByteString
encodeSessionEventData = \case
  (Connecting :=> Identity v) -> encode v
  (Connected :=> Identity v) -> encode v
  (FailedToConnect :=> Identity v) -> encode v
  (ReceivedUninterpretedMessage :=> Identity v) -> encode v
  (SessionUnableTo :=> Identity v) -> encode v
  (SentQuit :=> Identity v) -> encode v
  (EndedConnection :=> Identity v) -> encode v
  (LostConnection :=> Identity v) -> encode v
  (EndedSession :=> Identity v) -> encode v

encodeSessionEvent :: SessionEvent -> ByteString
encodeSessionEvent = \case
  (Connecting :=> Identity v) -> encodeAndTagWith "Connecting." v
  (Connected :=> Identity v) -> encodeAndTagWith "Connected." v
  (FailedToConnect :=> Identity v) -> encodeAndTagWith "FailedToConnect." v
  (ReceivedUninterpretedMessage :=> Identity v) -> encodeAndTagWith "ReceivedUninterpretedMessage." v
  (SessionUnableTo :=> Identity v) -> encodeAndTagWith "SessionUnableTo." v
  (SentQuit :=> Identity v) -> encodeAndTagWith "SentQuit." v
  (EndedConnection :=> Identity v) -> encodeAndTagWith "EndedConnection." v
  (LostConnection :=> Identity v) -> encodeAndTagWith "LostConnection." v
  (EndedSession :=> Identity v) -> encodeAndTagWith "EndedSession." v
  
  where
    encodeAndTagWith :: Serialise a => ByteString -> a -> ByteString
    encodeAndTagWith t = BS.append t . encode
-}
