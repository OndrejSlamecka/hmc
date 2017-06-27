module Hmc.CommonSpec (main, spec) where

import Protolude
import Hmc.Common
import Test.Hspec
import qualified Brick.Widgets.List as L
import Data.Vector (fromList)


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "directoryOneUp" $ do
    it "on empty string returns empty string" $
      directoryOneUp "" `shouldBe` ""

    it "on \"anything\" returns empty string" $
      directoryOneUp "anything" `shouldBe` ""

    -- make sure . is not an exception to the above case
    it "on \".\" returns empty string" $
      directoryOneUp "." `shouldBe` ""

    it "on \"a/b\" returns \"a\"" $
      directoryOneUp "a/b" `shouldBe` "a"


  describe "listEndIndex" $ do
    it "on empty list returns -1" $
      listEndIndex (L.list () (fromList []) 1) `shouldBe` -1

    it "on ['a'] returns 0" $
      listEndIndex (L.list () (fromList ['a']) 1) `shouldBe` 0

    it "on ['a', 'b'] returns 1" $
      listEndIndex (L.list () (fromList ['a', 'b']) 1) `shouldBe` 1
