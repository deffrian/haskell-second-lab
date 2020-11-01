module ServerSpec where


import Network.Wai.Handler.Warp
import Control.Concurrent.Map
import Control.Monad.IO.Class
import Data.Atomics.Counter
import Network.HTTP.Client
import Servant.Client
import Data.Hashable
import UserAPI
import Server as S
import Client as C
import Test.Hspec

no = Empty

diagonalLeft = Board 3 [[X, no, no], [no, X, no], [no, no, X]]
diagonalRight = Board 3 [[no, no, X], [no, X, no], [X, no, no]]

horizontal1 = Board 3 [[X, X, X], [no, no, no], [no, no, no]]
horizontal2 = Board 3 [[no, no, no], [X, X, X], [no, no, no]]

vertical1 = Board 3 [[X, no, no], [X, no, no], [X, no, no]]
vertical2 = Board 3 [[no, X, no], [no, X, no], [no, X, no]]

draw1 = Board 3 [[X, O, O], [O, X, X], [O, X, O]]
draw2 = Board 3 [[O, X, X], [X, O, O], [X, O, X]]

spec :: Spec
spec = do
  describe "isEnd" $ do
    it "empty" $ do
      isEnd (makeBoard 3) `shouldBe` SContinue
    it "diagonal" $ do
      isEnd diagonalLeft `shouldBe` SWinner
      isEnd diagonalRight `shouldBe` SWinner
    it "horisontal" $ do
      isEnd horizontal1 `shouldBe` SWinner
      isEnd horizontal2 `shouldBe` SWinner
    it "vertical" $ do
      isEnd vertical1 `shouldBe` SWinner
      isEnd vertical2 `shouldBe` SWinner
    it "draw" $ do
      isEnd draw1 `shouldBe` SDraw
      isEnd draw2 `shouldBe` SDraw