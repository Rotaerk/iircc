{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Concurrent.Async
import Pipes

import Irc.Commands
import Irc.Message
import Irc.RawIrcMsg

import qualified IRC

-- Run a test IRC daemon with:  ngircd -f ~/ngircd.conf -n

main :: IO ()
main = do
  c <- IRC.connect "localhost" "6667"
  runEffect $ do
    IRC.toConnection c <-< do
      IRC.send $ ircNick "rotaerk"
      IRC.send $ ircUser "rotaerk" False False "Matt"
    for (IRC.fromConnection c) $ liftIO . print
    IRC.toConnection c <-< IRC.close
  wait $ IRC.connectionTask c
