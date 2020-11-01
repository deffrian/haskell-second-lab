{-# LANGUAGE DataKinds #-}

module Main where

import Network.Wai.Handler.Warp
import UserAPI
import Server
import Control.Concurrent.Map
import Data.Atomics.Counter
import Data.Hashable

main :: IO ()
main = do
  m <- empty
  cnt <- newCounter 0
  let st = ServerState m cnt
  run 2282 (app st)

