{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

--import Network.IIRCC

import Control.Concurrent.Async
import Control.Monad.IO.Class

import Pipes
import Pipes.Concurrent as PC

import qualified Connection as Conn

-- Run a test IRC daemon with:  ngircd -n

main :: IO ()
main = do
  connection <- Conn.open "localhost" "6667" 4096
  runEffect $ do
    Conn.to connection <-< do
      yield $ Conn.Send "NICK rotaerk\r\n"
      yield $ Conn.Send "USER rotaerk 0 * :Matt\r\n"
    for (Conn.from connection) $ liftIO . print
  wait $ Conn.task connection

data IrcMessage = IrcMessage deriving (Show)

data SessionCommand = Send IrcMessage | Disconnect | Quit deriving (Show)
data SessionEvent = Received IrcMessage | Disconnected | Ended deriving (Show)
