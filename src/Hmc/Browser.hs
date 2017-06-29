{-# LANGUAGE OverloadedStrings #-}

module Hmc.Browser
  ( renderBrowser
  , browserViewEvent
  , saveTraversal
  , loadSavedTraversal
  , reloadDirectory
  , enterDirectory
  , loadDirectoryToPlaylist
  ) where

import Protolude hiding (State)
import Hmc.Common
import Hmc.Types
import Hmc.Playlist
import Data.Text (pack, unpack)
import Data.List (elemIndex)
import Data.Vector (fromList)
import Data.String (fromString)
import System.FilePath ((</>), takeDirectory, takeBaseName)
import Lens.Micro ((.~), (^.), (%~), (<&>))
import qualified Network.MPD
import qualified System.Directory as D
import qualified Network.MPD as MPD
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Brick.Widgets.Border (hBorderWithLabel)
import Brick.Widgets.Core
  ( txt
  , vBox
  )


-- | Render the list. Directory "" is interpreted as "All Music" item
renderBrowser title appState = vBox
  [ hBorderWithLabel (txt $ " " <> title <> " " )
  , L.renderList renderItem True (appState ^. browserList)
  ]
  where
    renderItem selected (MPD.LsDirectory "") = txt "All Music"
    renderItem selected (MPD.LsDirectory p)  = txt (MPD.toText p)
    renderItem selected (MPD.LsSong p) = txt (MPD.toText $ MPD.sgFilePath p)
    -- TODO: playlist


-- | Returns the path of the file where traversal in browser is saved
-- between hmc runs
getTraversalFilePath :: MonadIO m => m FilePath
getTraversalFilePath = liftIO $ D.getXdgDirectory D.XdgData "hmc/traversal"


-- | Saves position in browser to the file
saveTraversal state = do
  filepath <- getTraversalFilePath
  e <- liftIO . try $ createDirectory filepath :: MonadIO m => m (Either IOException ())
  case e of
    Left err -> return ()
    Right _  -> do
      liftIO $ writeFile filepath $ pack (state ^. traversal)
      return ()
  where
    createDirectory path = D.createDirectoryIfMissing True (takeDirectory path)


-- | Loads saved browser traversal into state
--
-- In case the file cannot be parsed or the directory cannot be loaded
-- by MPD traversal then stack starts from the root.
loadSavedTraversal :: MonadIO m => State -> m State
loadSavedTraversal state = do
  filepath <- getTraversalFilePath
  traversalFileExists <- liftIO $ D.doesFileExist filepath
  if not traversalFileExists
    then return state
    else do
      savedTraversal <- liftIO (unpack <$> readFile filepath)
      -- TODO: here we assume that savedTraversal is a correct path,
      -- that should be checked
      runLoader state (loadDirectory (fromString savedTraversal) (const 0))


-- | Loads the selected directory and updates traversal stack
loadDirectory :: MPD.Path -> ([MPD.LsResult] -> Int) -> State -> MPD.MPD State
loadDirectory dir position st = do
  list <- MPD.lsInfo dir
  -- In the topmost directory add "All Music" item
  let list' = if dir == ""
                then addAllItem list
                else list
  return $ st
    & browserListUnderlying .~ list'
    & browserList .~
      L.listMoveTo (position list') (L.list Browser (fromList list') 1)
    & traversal .~ MPD.toString dir
  where
    addAllItem = (MPD.LsDirectory "" :)


-- | Reloads the current directory
reloadDirectory :: MonadIO m => State -> m State
reloadDirectory st = runLoader st (loadDirectory currentDirectory position)
  where
    currentDirectory = fromString $ st ^. traversal
    position list =
      case selectedDirMay of
        Nothing       -> 0
        Just selected -> fromMaybe 0
                         (selected `elemIndex` list)
    selectedDirMay = snd <$> L.listSelectedElement (st ^. browserList)


-- | Move one up in the file tree
leaveDirectory :: MonadIO m => State -> m State
leaveDirectory st =
  if thisDirectory == directoryAbove
    then return st
    else runLoader st (loadDirectory (fromString directoryAbove) position)
  where
    directoryAbove = directoryOneUp thisDirectory
    thisDirectory = st ^. traversal
    position list = fromMaybe 0
                  $ thisDirectory `elemIndex` map lsResultPath list


-- | Enters the directory chosen in browser.
-- If there's nothing chosen does nothing,
-- if something else than a directory is chosen, does nothing.
enterDirectory :: MonadIO m => State -> m State
enterDirectory state = maybe (return state) enter selection
  where
    selection :: Maybe MPD.LsResult
    selection = snd <$> L.listSelectedElement (state ^. browserList)
    enter (MPD.LsDirectory dir) = runLoader state (loadDirectory dir (const 0))
    enter _                     = return state


-- | Load selected item into the playlist
loadDirectoryToPlaylist :: MonadIO m => State -> m State
loadDirectoryToPlaylist st = maybe (return st) modifier selection
  where
    selection = L.listSelectedElement (st ^. browserList)
    modifier (_, path) = modifyState st updatePlaylist
                     <$> modifyPlaylist st path
    updatePlaylist _ (list, state) = leaveBrowser state
      & playlist .~ L.list Playlist (fromList list) 1
    leaveBrowser state =
      if state ^. appView == BrowserView BrowserAdd
        then state
        else state & appView .~ PlaylistView


-- | Handles events specific for the browser view.
-- Returns Nothing if there is no behavior specified for the event.
browserViewEvent :: State
                 -> T.BrickEvent WidgetName Event
                 -> Maybe (T.EventM WidgetName (T.Next State))
browserViewEvent st (T.VtyEvent e) = case e of
  V.EvKey (V.KChar 'g') [] -> Just $ M.continue =<< handleKeyCombo 'g' 'g' move st
    where move st = return $ st & browserList %~ L.listMoveTo 0
  V.EvKey V.KLeft [] -> Just $ M.continue =<< leaveDirectory st
  V.EvKey V.KRight [] -> Just $ M.continue =<< enterDirectory st
  V.EvKey V.KEnter [] -> Just $ M.continue =<< loadDirectoryToPlaylist st
  _ -> Nothing
browserViewEvent st ev = Nothing
