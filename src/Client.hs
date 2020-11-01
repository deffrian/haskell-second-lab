module Client where

import UserAPI
import Brick
import Servant
import Servant.Client
import Network.HTTP.Client
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty
import Control.Monad.IO.Class

newGame :: Maybe Int -> ClientM Int
move :: Int -> Move -> ClientM MoveResult

newGame :<|> move = client userAPI

clientEnv :: IO ClientEnv
clientEnv = do
  m <- newManager defaultManagerSettings
  return $ mkClientEnv m (BaseUrl Http "localhost" 2282 "")

data GameResult = Continue | WinRes | LossRes | DrawRes deriving (Eq, Show)

data StateType = StateType { stBoard :: Board, stPos :: Move, stResult :: GameResult } deriving Show

selected :: AttrName
selected = attrName "selected"

gameAttrMap :: AttrMap
gameAttrMap =
  attrMap
    defAttr
    [(selected, fg red)]

moveLeft :: StateType -> StateType
moveLeft (StateType board (Move x y) res) = StateType board (Move x (max 0 (y - 1))) res

moveRight :: StateType -> StateType
moveRight (StateType board@(Board n _) (Move x y) res) = StateType board (Move x (min n (y + 1))) res

moveUp :: StateType -> StateType
moveUp (StateType board (Move x y) res) = StateType board (Move (max 0 (x - 1)) y) res

moveDown :: StateType -> StateType
moveDown (StateType board@(Board n _) (Move x y) res) = StateType board (Move (min n (x + 1)) y) res

makeMove :: CellState -> Board -> Move -> (Bool, Board)
makeMove val (Board size board) syn@(Move x y) = do
  let (changed, board') = helper x board
  (changed, Board size board') where
  helper :: Int -> [[CellState]] -> (Bool, [[CellState]])
  helper 1 (l : ls) = do
    let (changed, res) = helper2 y l
    (changed, res : ls)
  helper n (l : ls) = do
    let (changed, res) = helper (n - 1) ls
    (changed, l : res)
  helper2 :: Int -> [CellState] -> (Bool, [CellState])
  helper2 1 (e : es)
    | e == Empty = (True, val : es)
    | otherwise  = (False, e : es)
  helper2 m (e : es) = do
    let (changed, res) = helper2 (m - 1) es
    (changed, e : res)

isGameEnded :: StateType -> Bool
isGameEnded st = stResult st /= Continue

handleEvent :: Int -> StateType -> BrickEvent String () -> EventM String (Next StateType)
handleEvent gameId syn@(StateType board mv Continue) (VtyEvent (EvKey KEnter [])) = do
  let (changed, board') = makeMove X board mv
  if changed then do
    e <- liftIO clientEnv
    res <- liftIO $ runClientM (move gameId mv) e
    case res of
      (Left _) -> error "Unexpected error"
      (Right (ComputerMove compMove)) -> do
        let (_, board'') = makeMove O board' compMove
        continue (StateType board'' mv Continue)
      (Right (End Human)) -> do
        continue (StateType board' mv WinRes)
      (Right (End (Draw compMove))) -> do
        let (_, board'') = makeMove O board' compMove
        continue (StateType board'' mv DrawRes)
      (Right (End (Computer compMove))) -> do
        let (_, board'') = makeMove O board' compMove
        continue (StateType board'' mv LossRes)
  else continue syn
handleEvent _ st (VtyEvent (EvKey (KChar 'q') [])) = halt st
handleEvent _ st (VtyEvent (EvKey KUp []))
  | isGameEnded st = continue st
  | otherwise      = continue $ moveUp st
handleEvent _ st (VtyEvent (EvKey KRight []))
  | isGameEnded st = continue st
  | otherwise      = continue $ moveRight st
handleEvent _ st (VtyEvent (EvKey KDown []))
  | isGameEnded st = continue st
  | otherwise      = continue $ moveDown st
handleEvent _ st (VtyEvent (EvKey KLeft []))
  | isGameEnded st = continue st
  | otherwise      = continue $ moveLeft st
handleEvent _ st _ = continue st

app :: Int -> App StateType () String
app gameId = App ui neverShowCursor (handleEvent gameId) return (const gameAttrMap)

drawGrid :: StateType -> Widget String
drawGrid (StateType (Board size board) (Move x y) Continue) = vBox $ grid 1 board
  where
    grid _ [] = []
    grid n (l : ls) = hBox (drowLine n 1 l) : grid (n + 1) ls
    drowLine _ _ [] = []
    drowLine n m (e : es) = showCell n m e : drowLine n (m + 1) es
    --grid n = map (hBox . map showCell) board
    showCell n m
      | n /= x || m /= y = toStr
      | otherwise        =  (withAttr selected) . toStr
    toStr X = str " X "
    toStr O = str " O "
    toStr Empty = str " . "
drawGrid (StateType _ _ DrawRes) = center (str "Draw")
drawGrid (StateType _ _ LossRes) = center (str "Loss")
drawGrid (StateType _ _ WinRes) = center (str "Win")


ui :: StateType -> [Widget String]
ui st = [center $ drawGrid st]