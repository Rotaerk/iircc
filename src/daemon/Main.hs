{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Concurrent.Async
import Control.Exception
import Data.Functor.Contravariant
import Data.Text
import GHC.IO.Exception

import Pipes
import Pipes.Concurrent as PC
import Pipes.Exhaustion
import qualified Pipes.Prelude as PP

import Irc.Commands
import Irc.Message
import Irc.RawIrcMsg

import qualified IIRCC.IRC.Session as IRCSession

-- Run a test IRC daemon with:  ngircd -f ~/ngircd.conf -n

main :: IO ()
main = do
  (messageSink, messageSource, sealMailbox) <- spawn' unbounded
  session <- IRCSession.start "localhost" "6667" (contramap SessionEventReport messageSink)
  inputReceiver <- async $ runEffect $ PP.stdinLn >-> toOutput (contramap UserInput messageSink)
  runEffect $ inexhaustible (fromInput messageSource) >-> runEffect (clientPipe >-> PP.stdoutLn) >-> toOutput (IRCSession.commandSink session)
  atomically sealMailbox

data ClientMessage =
  SessionEventReport IRCSession.EventReport |
  UserInput String
  deriving (Show)
  
clientPipe :: Producer String (Pipe ClientMessage IRCSession.Command IO) ()
clientPipe = awaitClientMessage >>= \case
  SessionEventReport n -> case n of
    IRCSession.Connecting h p -> do
      yieldClientOutput $ "Connecting to '" ++ h ++ "' port " ++ p ++ "..."
      clientPipe
    IRCSession.Connected -> do
      yieldClientOutput $ "Connected."
      yieldSessionCommand $ IRCSession.SendMessage $ ircNick "rotaerk"
      yieldSessionCommand $ IRCSession.SendMessage $ ircUser "rotaerk" False False "Matt"
      clientPipe
    IRCSession.FailedToConnect ioe -> do
      yieldClientOutput $ "Failed to connect: " ++ ioe_description ioe
      clientPipe
    IRCSession.ReceivedMessage (IRCSession.ValidIrcMessage m) -> do
      yieldClientOutput $ "Received message: " ++ show m
      case m of
        Ping servers -> yieldSessionCommand $ IRCSession.SendMessage $ ircPong servers
        _ -> return ()
      clientPipe
    IRCSession.ReceivedMessage (IRCSession.InvalidIrcMessage t) -> do
      yieldClientOutput $ "Received invalid message: " ++ show t
      clientPipe
    IRCSession.SentMessage m -> do
      yieldClientOutput $ "Sent message: " ++ show m
      clientPipe
    IRCSession.UnableTo command reason -> do
      yieldClientOutput $ "Failed to " ++ show command ++ ": " ++ show reason
      clientPipe
    IRCSession.Disconnected -> do
      yieldClientOutput "Disconnected."
      clientPipe
    IRCSession.LostConnection Nothing -> do
      yieldClientOutput "Lost connection with no error."
      clientPipe
    IRCSession.LostConnection (Just ioe) -> do
      yieldClientOutput $ "Lost connection: " ++ ioe_description ioe
      clientPipe
    IRCSession.Ended -> yieldClientOutput "Exiting..."
  UserInput s -> case s of
    "/connect" -> do
      yieldSessionCommand IRCSession.Connect
      clientPipe
    "/quit" -> do
      yieldSessionCommand $ IRCSession.SendMessage $ ircQuit "Quitting"
      clientPipe
    "/close" -> do
      yieldSessionCommand IRCSession.Close
      clientPipe
    "/disconnect" -> do
      yieldSessionCommand IRCSession.Disconnect
      clientPipe
    "" -> clientPipe
    i -> do
      liftIO $ putStrLn $ "Unexpected Input: " ++ i
      clientPipe

  where
    awaitClientMessage = lift await
    yieldClientOutput = yield
    yieldSessionCommand = lift . yield
