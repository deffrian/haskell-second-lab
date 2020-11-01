{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module UserAPI where

import Servant
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import Network.Wai.Handler.Warp
import Web.HttpApiData

-- | type that represents cell state
data CellState
  = O     -- ^ marked as server choose
  | X     -- ^ marked as player choose
  | Empty -- ^ not marked
  deriving (Eq, Show, Generic)

-- | type that represents game board
data Board = Board
  { boardSize :: Int          -- ^ size of board
  , curState :: [[CellState]] -- ^ board
  } deriving Show

-- | type with cells coordinates
data Move = Move
  { x :: Int
  , y :: Int
  } deriving (Read, Show, Eq, Generic)

-- | type to represent server move
data Winner
  = Computer Move -- ^ server win last move
  | Human         -- ^ player win
  | Draw Move     -- ^ draw last move (Move 0 0 if player did last move)
  deriving (Read, Show, Eq, Generic)

-- | type to represent server move
data MoveResult
  = ComputerMove Move -- ^ next servers move
  | End Winner        -- ^ end of game
  deriving (Read, Show, Eq, Generic)

$(deriveJSON defaultOptions ''CellState)

$(deriveJSON defaultOptions ''Move)

$(deriveJSON defaultOptions ''Winner)

$(deriveJSON defaultOptions ''MoveResult)

-- | type that represent server api
type UserAPI =
  "new_game" :> QueryParam "size" Int :> Get '[JSON] Int :<|>
  "move" :> Capture "gameId" Int :> ReqBody '[JSON] Move :> Post '[JSON] MoveResult

userAPI :: Proxy UserAPI
userAPI = Proxy