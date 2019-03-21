{-# LANGUAGE OverloadedStrings #-}

module Hmc.Search
  ( handleSearchEvent
  , createSearchInput
  , renderSearchContent
  ) where

import Protolude hiding (State)
import Hmc.Common
import Hmc.Types
import Hmc.Playlist
import Hmc.Browser
import Data.Text (pack, unpack, toLower, isInfixOf)
import Data.List (elemIndex)
import Data.Vector (fromList)
import Data.String (fromString)
import System.FilePath (takeBaseName)
import Lens.Micro ((.~), (^.), (%~))
import qualified System.Directory as D
import qualified Network.MPD as MPD
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Brick.Widgets.Core (txt)


-- | The search widget
createSearchInput :: E.Editor Text WidgetName
createSearchInput = E.editor Search (Just 1) ""


-- | Rendering
renderSearchContent :: [Text] -> T.Widget a
renderSearchContent = txt . fromMaybe "" . head


-- | Loads contents into a list based on the query
searchLoader :: View -> Text -> State -> MPD.MPD State
searchLoader PlaylistView search state = do
  pl <- MPD.playlistSearch (MPD.Title MPD.=? search')
  return $ state & playlist .~ L.list Playlist (fromList pl) 1
  where search' = fromString . unpack $ search

searchLoader _ search state = return $ state
  & browserList .~ L.list Browser (fromList l) 1
  where
    l = filter predicate (state ^. browserListUnderlying)
    predicate = match search . fromString . lsResultPath


-- | Destroys the search widget and returns to showing the whole list
leaveSearch :: MonadIO m => View -> State -> m State
leaveSearch PlaylistView state = do
  state' <- runLoader state (searchLoader (state ^. appView) "")
  let chosenSong = fromMaybe 0 (join $ MPD.sgIndex <$> state' ^. currentSong)
  return $ state'
    & playlist %~ L.listMoveTo chosenSong
    & searchInput .~ Nothing

leaveSearch _ state = return $ state
  & browserList .~ L.list Browser v 1
  & searchInput .~ Nothing
  where v = fromList (state ^. browserListUnderlying)


-- | Updates the state based on the given event. If the event is not
-- related to searching returns Nothing.
handleSearchEvent :: State
                  -> T.BrickEvent WidgetName Event
                  -> E.Editor Text WidgetName
                  -> Maybe (T.EventM WidgetName (T.Next State))
handleSearchEvent state (T.VtyEvent event) input =
  case event of
    V.EvKey V.KEsc [] -> Just $ M.continue =<< leaveSearch (state ^. appView) state
    V.EvKey V.KRight [] ->
      case state ^. appView of
        PlaylistView  -> Nothing
        BrowserView _ -> Just
                       $ enterDirectory state
                     >>= leaveSearch (state ^. appView)
                     >>= M.continue
    V.EvKey V.KUp [] -> Nothing
    V.EvKey V.KDown [] -> Nothing
    V.EvKey V.KEnter [] ->
      case state ^. appView of
        PlaylistView  -> Just
                       $ playSelectedSong state
                     >>= leaveSearch (state ^. appView)
                     >>= M.continue
        BrowserView _ -> Just
                       $ loadDirectoryToPlaylist state
                     >>= leaveSearch (state ^. appView)
                     >>= M.continue
        _             -> Just $ M.continue state
    V.EvKey (V.KChar c) [] -> Just $ M.continue =<< updateSearch
    V.EvKey V.KBS []       -> Just $ M.continue =<< updateSearch
    V.EvKey V.KDel []      -> Just $ M.continue =<< updateSearch
    _ -> Just $ M.continue =<< do
      input' <- E.handleEditorEvent event input
      return $ state & searchInput .~ Just input'

  where
    updateSearch = do
      input' <- E.handleEditorEvent event input
      let stateNewInput = state & searchInput .~ Just input'
      let search = fromMaybe "" . head $ E.getEditContents input'
      runLoader stateNewInput (searchLoader (state ^. appView) search)

handleSearchEvent state _ input = Nothing


-- | Matches against the files' base name, case insensitive, looks for
-- match anywhere
-- TODO: Get rid of the unpack and pack to speed it up.
match needle haystack = toLower needle `isInfixOf` toLower base
  where base = pack . takeBaseName . unpack $ haystack

