module Pipes.Exhaustion (
  inexhaustible
) where

import Control.Exception
import Pipes
import Pipes.Core

inexhaustible :: Monad m => Proxy a' a b' b m r -> Proxy a' a b' b m x
inexhaustible p = p *> (throw . AssertionFailed $ "Inexhaustible proxy exhausted.")
