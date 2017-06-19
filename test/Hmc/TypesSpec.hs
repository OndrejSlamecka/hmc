module Hmc.TypesSpec (main, spec) where

import Protolude
import Hmc.Types
import Test.Hspec
import Test.QuickCheck
import Data.Text (unpack)
import qualified Network.MPD as MPD
import Data.String


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "parseTraversalStack" $ do
    it "is inverse of printTraversalStack" $ property $
      \x -> (x == (ps . parseTraversalStack . printTraversalStack . sp) x)
  where
    sp = map (\(Positive i, s) -> (i, fromString s :: MPD.Path))
    ps = map (\(i, p) -> (Positive i, MPD.toString p))
