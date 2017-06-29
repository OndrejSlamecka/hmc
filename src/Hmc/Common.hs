module Hmc.Common
  ( (%%~)
  , directoryOneUp
  , listEndIndex
  , handleKeyCombo
  , lsResultPath
  , stateFromEither
  , runLoader
  , runAction
  , modifyState
  , loadState
  ) where

import Protolude hiding (State)
import Hmc.Types
import Data.Text (unpack)
import Data.List (elemIndex)
import Data.Vector (fromList, length)
import Data.String (fromString)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Lens.Micro ((.~), (^.), (%~), (<&>))
import System.FilePath ((</>), takeDirectory)
import qualified Network.MPD
import qualified System.Directory as D
import qualified Network.MPD as MPD
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Types as T
import qualified Graphics.Vty as V


-- | Microlens doesn't have this
(%%~) = identity


-- | Like takeDirectory but replaces "." with "",
-- to make it compatible with MPD's lsInfo
directoryOneUp :: FilePath -> FilePath
directoryOneUp d = if oneUp == "." then "" else oneUp
  where oneUp = takeDirectory d


-- | Returns the last index in the given list (in O(1) time)
listEndIndex :: L.List n e -> Int
listEndIndex list = Data.Vector.length (list ^. L.listElementsL) - 1


-- | Generic handler for quick successive presses.
-- firstLetter is the expected first letter in the combo,
-- secondLetter is the one just pressed.
handleKeyCombo :: MonadIO m => Char -> Char -> (State -> m State) -> State -> m State
handleKeyCombo firstLetter secondLetter action st = liftIO getPOSIXTime >>= go
  where
    go now = if combo
      then action st <&> keyCombo .~ Nothing
      else return $ st & keyCombo .~ Just (secondLetter, now)
      where
        combo = case st ^. keyCombo of
          Nothing -> False
          Just (lastKey, lastKeyTime) -> lastKey == firstLetter && lastKeyTime > now - 1


-- | Extracts path from MPD's LsResult
lsResultPath :: MPD.LsResult -> FilePath
lsResultPath (MPD.LsSong p) = MPD.toString . MPD.sgFilePath $ p
lsResultPath (MPD.LsDirectory p) = MPD.toString p
lsResultPath (MPD.LsPlaylist p) = MPD.toString p


-- | Load status and current song from MPD
loadState :: State -> MPD.MPD State
loadState state = do
  status' <- MPD.status
  currentSong' <- MPD.currentSong

  return $ state
    & playingStatus .~ status'
    & currentSong .~ currentSong'


-- Application State Management


-- | Returns either the old state with mpdError updated,
-- does not remove error if present
stateFromEither :: State -> MPD.Response State -> State
stateFromEither oldSt = modifyState oldSt (\oldSt' newSt -> newSt)


-- | Load information using given loader from MPD, run the MPD monad and
-- extract the contents of the response (i.e. add error to old state on
-- error, or replace it with the new state)
runLoader :: MonadIO m =>
  State -> (State -> MPD.MPD State) -> m State
runLoader st loader = stateFromEither st <$> liftIO (MPD.withMPD $ loader st)


-- | Runs an MPD action and updates the mpdError in app state
runAction :: MonadIO m =>
  State -> (State -> MPD.MPD a) -> m State
runAction st loader = runLoader st (\st' -> loader st' >> return st')


-- | In case MPD result is an error, returns the state with error,
-- otherwise returns the state after applying a modifier which gets
-- passed the state and result contents
modifyState :: State -> (State -> a -> State) -> MPD.Response a -> State
modifyState state modifier either = case either of
  Left err -> state & mpdError .~ Just err
  Right result -> modifier state result
