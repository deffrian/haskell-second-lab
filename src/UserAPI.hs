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

data CellState = O | X | Empty deriving (Eq, Show, Generic)

data Board = Board { boardSize :: Int, curState :: [[CellState]]}

data Move = Move
  { x :: Int
  , y :: Int
  } deriving (Read, Show, Eq, Generic)

data Winner
  = Computer Move -- ^ last move
  | Human
  | Draw Move
  deriving (Read, Show, Eq, Generic)

data MoveResult = ComputerMove Move | End Winner deriving (Read, Show, Eq, Generic)

$(deriveJSON defaultOptions ''CellState)

$(deriveJSON defaultOptions ''Move)

$(deriveJSON defaultOptions ''Winner)

$(deriveJSON defaultOptions ''MoveResult)

type UserAPI =
  "new_game" :> QueryParam "size" Int :> Get '[JSON] Int :<|>
  "move" :> Capture "gameId" Int :> ReqBody '[JSON] Move :> Post '[JSON] MoveResult

userAPI :: Proxy UserAPI
userAPI = Proxy