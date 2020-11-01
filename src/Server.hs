{-# LANGUAGE BangPatterns #-}

module Server where

import Servant
import Network.Wai.Handler.Warp
import UserAPI
import Control.Concurrent.Map as CM
import Data.Atomics.Counter
import Data.Hashable
import Control.Monad.IO.Class
import Data.List as L
import Data.Maybe
import System.IO

instance Hashable CellState where
  hash O = 1
  hash X = 2
  hash Empty = 3

data ServerState = ServerState { allGames :: Map Int Board, gamesCnt :: AtomicCounter }

data BoardState = SDraw | SWinner | SContinue deriving (Show, Eq)

isEnd :: Board -> BoardState
isEnd (Board size board)
  | isVertical board || isHorizontal size || isDiagonal = SWinner
  | isDraw = SDraw
  | otherwise = SContinue
  where
  isVertical [] = False
  isVertical (l : ls) = all (==X) l || all (==O) l || isVertical ls

  isHorizontal 0 = False
  isHorizontal n = isHorizontalHelper (n - 1) X board || isHorizontalHelper (n - 1) O board || isHorizontal (n - 1)

  isHorizontalHelper _ _ [] = True
  isHorizontalHelper n val (l : ls) = l !! n == val && isHorizontalHelper n val ls

  isDiagonal =
    all (\n -> (board !! n) !! n == X) [0..(size - 1)] || all (\n -> (board !! n) !! n == O) [0..(size - 1)] ||
    all (\n -> (board !! n) !! (size - n -1) == X) [0..(size - 1)] || all (\n -> (board !! n) !! (size - n - 1) == O) [0..(size - 1)]
    
  isDraw = not $ any (elem Empty) board

makeBoard :: Int -> Board
makeBoard n = Board n (replicate n (replicate n Empty))

makeHumanMove :: Move -> Board -> Handler Board
makeHumanMove (Move x y) (Board size board)
  | x > size || x < 1 ||
    y > size || y < 1 = throwError err400
  | otherwise         = do
    board' <- helper x board
    return $ Board size board'
    where
    helper :: Int -> [[CellState]] -> Handler [[CellState]]
    helper 1 (l : ls) = do
      l' <- helper2 y l
      return $ l' : ls
    helper n (l : ls) = do
      ls' <- helper (n - 1) ls
      return $ l : ls'
    helper2 :: Int -> [CellState] -> Handler [CellState]
    helper2 1 (e : es)
      | e /= Empty = throwError err400
      | otherwise  = return $ X : es
    helper2 n (e : es) = do
      es' <- helper2 (n - 1) es
      return $ e : es'

makeMove :: Board -> Move -> Handler (MoveResult, Board)
makeMove syn@(Board size board) mv = do
  syn'@(Board _ board') <- makeHumanMove mv syn
  case isEnd syn' of
    SWinner   -> return (End Human, syn')
    SDraw     -> return (End $ Draw $ Move 0 0, syn')
    SContinue -> do
      let (compMv, board'') = makeComputerMove 1 board'
      let syn'' = Board size board''
      case isEnd syn'' of
        SWinner   -> return (End $ Computer compMv, syn'')
        SDraw     -> return (End $ Draw compMv, syn'')
        SContinue -> return (ComputerMove compMv, syn'')
  where
  makeComputerMove :: Int -> [[CellState]] -> (Move, [[CellState]])
  makeComputerMove _ [] = (Move 0 0, [])
  makeComputerMove !n syn@(l : ls) = do
    let (idx, l') = replaceAndGetPos 1 l
    case idx of
      Nothing -> do
        let (res, ls') = makeComputerMove (n + 1) ls
        (res, l : ls')
      (Just m) -> (Move n m, l' : ls)
  replaceAndGetPos _ [] = (Nothing, [])
  replaceAndGetPos !n (e : es)
    | e == Empty = (Just n, O : es)
    | otherwise  = do
      let (res, es') = replaceAndGetPos (n + 1) es
      (res, e : es')

moveHandler :: ServerState -> Int -> Move -> Handler MoveResult
moveHandler st gameId mv = do
  game <- liftIO $ CM.lookup gameId (allGames st)
  game' <- case game of
    Nothing -> throwError err400
    (Just val) -> return val
  (compMv, newBoard) <- makeMove game' mv
  case compMv of
    (End _) -> do
      _ <- liftIO $ CM.delete gameId (allGames st)
      return compMv
    (ComputerMove _) -> do
      _ <- liftIO $ CM.insert gameId newBoard (allGames st)
      return compMv

newGameHandler :: ServerState -> Maybe Int -> Handler Int
newGameHandler st size = do
  realSize <- case size of
    Nothing  -> return 3
    (Just n) -> return (if n < 3 then 3 else n)
  gameId <- liftIO $ incrCounter 1 (gamesCnt st)
  liftIO $ CM.insert gameId (makeBoard realSize) (allGames st)
  return gameId

server :: ServerState -> Server UserAPI
server st = newGameHandler st :<|> moveHandler st

app :: ServerState -> Application
app st = serve userAPI (server st)