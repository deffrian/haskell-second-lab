{-# LANGUAGE DataKinds #-}

module Main where

import Network.Wai.Handler.Warp
import UserAPI
import Server as S
import Client as C
import Servant.Client
import Network.HTTP.Client
import Brick
import Control.Concurrent.Map
import Data.Atomics.Counter
import Data.Hashable
import System.Environment

-- | Entry point
-- | client UI:
-- |    q - quit
-- |    Enter - mark empty cell with 'X'
-- |    Up / Down / Left / Right - move cursor to select cell
-- | server runs on 2282 port
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["server"] -> do
      m <- empty
      cnt <- newCounter 0
      let st = ServerState m cnt
      run 2282 (S.app st)
    ["client", gameSize] -> do
      let size = read gameSize
      e <- clientEnv
      gameId <- runClientM (newGame (Just size)) e
      case gameId of
        Left _ -> putStrLn "Error"
        Right gameId' -> do
          let board = Board size (replicate size (replicate size Empty))
          _ <- defaultMain (C.app gameId') (StateType board (Move 1 1) Continue)
          return ()
    _ -> do
      putStrLn "Usage: "
      putStrLn "  server            -- run server"
      putStrLn "  clienr grid_size  -- run client"