module Hmc.EventHandlerSpec (main, spec) where

import Protolude
import Hmc.EventHandler
import Test.Hspec
import qualified Brick.Widgets.List as L
import Data.Vector (fromList)


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "listEndIndex" $ do
    it "on empty list returns -1" $
      listEndIndex (L.list () (fromList []) 1) `shouldBe` -1

    it "on ['a'] returns 0" $
      listEndIndex (L.list () (fromList ['a']) 1) `shouldBe` 0

    it "on ['a', 'b'] returns 1" $
      listEndIndex (L.list () (fromList ['a', 'b']) 1) `shouldBe` 1
