{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Concurrent.Async
import Control.Exception
import GHC.IO.Exception

import Pipes

import Irc.Commands
import Irc.Message
import Irc.RawIrcMsg

import qualified IIRCC.IRC as IRC

-- Run a test IRC daemon with:  ngircd -f ~/ngircd.conf -n

main :: IO ()
main =
  try (IRC.connect "localhost" "6667") >>= \case
    Right c -> do
      runEffect $ do
        IRC.toConnection c <-< do
          IRC.send $ ircNick "rotaerk"
          IRC.send $ ircUser "rotaerk" False False "Matt"
        for (IRC.fromConnection c) $ liftIO . print
        IRC.toConnection c <-< IRC.close
      cleanExit <- wait $ IRC.connectionTask c
      putStrLn $ "Connection terminated " ++ (if cleanExit then "cleanly." else "by remote host.")
    Left e -> putStrLn $ "Failed to open the connection.  The error was: " ++ ioe_description e
