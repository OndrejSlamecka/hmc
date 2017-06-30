{-# LANGUAGE OverloadedStrings #-}

module Hmc.Playlist
  ( playingSongAttr
  , renderPlaylist
  , modifyPlaylist
  , loadPlaylist
  , play
  , playNext
  , playSelectedSong
  , playlistViewEvent
  ) where

import Protolude hiding (State)
import Hmc.Types
import Hmc.Common
import Data.Text (unpack, take)
import Data.List (elemIndex)
import Data.Vector (fromList)
import Data.String (fromString)
import System.Timer.Updatable (replacer, renewIO)
import Lens.Micro ((.~), (^.), (%~), (<&>))
import qualified Data.Text as Text
import qualified Data.Map.Lazy as Map
import qualified Network.MPD
import qualified System.Directory as D
import qualified Network.MPD as MPD
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.BChan as C
import qualified Brick.Types as T
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.List as L
import Brick.Widgets.Border (hBorder, hBorderWithLabel)
import Brick.Widgets.Core
  ( txt
  , vBox
  , hBox
  , withAttr
  , padRight
  , hLimit
  )


data SeekDirection = Backward | Forward


-- | Attribute for playing song in list
playingSongAttr :: A.AttrName
playingSongAttr = "playingSongAttr"


-- | Rendering
renderPlaylist appState = vBox
  [ hBorderWithLabel $ txt " Playlist "
  , L.renderList renderItem True (appState ^. playlist)
  ]
  where
    renderItem _ song = style song $ renderSong appState lastTag song
    style song = if MPD.sgId song == (appState ^. playingStatus . stSongIDL)
      then withAttr playingSongAttr
      else identity

    lastTag = fst <$> lastMay (appState ^. tagsAndWidths)


renderSong appState lastTag song
  | mode == PlaylistPaths = txt . MPD.toText . MPD.sgFilePath $ song
  | mode == PlaylistTags  = hBox $ map getTag (appState ^. tagsAndWidths)
  where
    mode = appState ^. playlistMode
    tags = MPD.sgTags song
    getTag (tag, maxWidth) =
      (if lastTag /= Just tag then hLimit width else identity)
      . padRight T.Max
      . txt
      . (if lastTag /= Just tag then Data.Text.take maxWidth else identity)
      . fromMaybe "NO TAG"
      $ MPD.toText <$> (head =<< Map.lookup tag tags)
      where
        width = (+spacing) . min maxWidth
          $ fromMaybe maxWidth (Map.lookup tag tagsMaxWidths)

    spacing = 3
    tagsMaxWidths = appState ^. playlistTagsMaxWidths


-- | Returns the position of the current song in the playlist or 0 if
-- there is no such song
currentSongPosition st = fromMaybe 0 maybePos
  where maybePos = join $ MPD.sgIndex <$> st ^. currentSong


-- | Takes a playlist and returns a map from tag to maximum length of
-- its value (or Nothing if there is no value)
playlistToTagsMaxWidths :: [(MPD.Metadata, Int)] -> [MPD.Song] -> Map MPD.Metadata Int
playlistToTagsMaxWidths tagsAndWidths playlist = fromMaybe Map.empty
  $ foldr1May (Map.unionWith max) tagLengthMaps
  where
    tagLengthMaps = map (tagsToLengths . MPD.sgTags) playlist
    -- If the list of values is empty the length is set to 20 to leave
    -- some space for an error text (this approach should be improved)
    tagsToLengths = Map.map (\vs -> fromMaybe 20 (Text.length . MPD.toText <$> head vs))


-- | Load playlist information from MPD
loadPlaylist :: State -> MPD.MPD State
loadPlaylist state = do
  playlistInfo' <- MPD.playlistInfo Nothing

  return $ state
    & playlist .~ L.list Playlist (fromList playlistInfo') 1
    & playlistTagsMaxWidths .~ playlistToTagsMaxWidths (state ^. tagsAndWidths) playlistInfo'


-- | Modify playlist and return the new playlist and state
--
-- The playlist is modified based on the given LsResult,
-- the "All Music" option is just "" dir so it works as usual.
--
-- State is modified just in case of open, not add, because new song has started
-- playing.
modifyPlaylist :: MonadIO m =>
     State -> MPD.LsResult -> m (MPD.Response ([MPD.Song], State))
modifyPlaylist st selection = liftIO $ MPD.withMPD $ do
  paths <- case selection of
      MPD.LsDirectory path -> return [path]
      MPD.LsSong song      -> return [MPD.sgFilePath song]
      MPD.LsPlaylist name  -> MPD.listPlaylist name

  when (st ^. appView == BrowserView BrowserOpen) (void MPD.clear)
  void $ forM_ paths MPD.add
  when (st ^. appView == BrowserView BrowserOpen) (MPD.play Nothing)

  plInfo <- MPD.playlistInfo Nothing

  newState <- if st ^. appView == BrowserView BrowserOpen
    then loadPlaylist st >>= loadState
    else return st

  return (plInfo, newState)


--- Playlist actions

-- | Play the given song.
play :: MonadIO m => State -> MPD.Song -> m State
play st song =
  -- Why is sgId Maybe?
  case MPD.sgId song of
    Nothing -> return st
    Just id -> runLoader st (loader id)
  where
    loader id st' = do
      MPD.playId id
      st'' <- loadState st'
      return $ st'' & playlist %~ L.listMoveTo (currentSongPosition st'')


-- | Play the next song.
playNext :: MonadIO m => State -> m State
playNext st = runLoader st loader
  where
    loader st' = do
      MPD.next
      st'' <- loadState st'
      return $ st'' & playlist %~ L.listMoveTo (currentSongPosition st'')


-- | If there's a song selected, plays it, otherwise returns the given
-- state.
playSelectedSong :: MonadIO m => State -> m State
playSelectedSong state = maybe (return state) (\(_, song) -> play state song) selection
  where
    selection :: Maybe (Int, MPD.Song)
    selection = L.listSelectedElement $ state ^. playlist


-- | Removes the selected song from the playlist
removeSelectedSong :: MonadIO m => State -> m State
removeSelectedSong state = maybe (return state) remove selectedMay
  where
    remove i    = do
      state' <- runAction state (return $ MPD.delete i)
      return $ state' & playlist %~ (L.listMoveDown . L.listRemove i)
    selectedMay = state ^. playlist . L.listSelectedL


-- | After a delay send an event signaling the player should seek to the
-- currently selected position.
-- The delay is restarted on repeated invocation if the event wasn't
-- dispatched yet.
scheduleSeekEvent :: MonadIO m => State -> m State
scheduleSeekEvent st = liftIO $
  case st ^. seekTimer of
    Nothing -> do
      timer <- replacer sendSeekEvent timerDelay
      return $ st & seekTimer .~ Just timer
    Just timer ->
      renewIO timer timerDelay >> return st
  where
    sendSeekEvent = C.writeBChan (st ^. eventChannel) Seek
    timerDelay = 2*10^5 -- 200 ms


-- | Seek a position in the currently playing song.
-- This happens locally and an event resulting in updating the MPD
-- server playing progress is scheduled (see `scheduleSeekEvent`).
seek :: MonadIO m => State -> SeekDirection -> m State
seek st direction =
  case join $ MPD.sgId <$> st ^. currentSong of
    Nothing -> return st
    Just id -> do
      st' <- scheduleSeekEvent st
      return $ st' & playingStatus . stTimeL .~ Just time'
  where
    time' = (pos', snd time)
    pos'  = max 0 (min (fromIntegral $ snd time - 1) (fst time + delta))
    pos   = fst time
    time  = fromMaybe (0,0) (st ^. playingStatus . stTimeL)
    delta = go direction
    go Forward  = 5
    go Backward = - go Forward


-- | Handles events specific for playlist. Returns Nothing if it cannot
-- be processed here.
playlistViewEvent :: State
                  -> T.BrickEvent WidgetName Event
                  -> Maybe (T.EventM WidgetName (T.Next State))
playlistViewEvent state (T.VtyEvent e) = case e of
  V.EvKey (V.KChar 'g') [] -> Just $ M.continue =<< handleKeyCombo 'g' 'g' move state
    where move st = return $ st & playlist %~ L.listMoveTo 0
  V.EvKey V.KLeft [] -> Just $ M.continue =<< seek state Backward
  V.EvKey V.KRight [] -> Just $ M.continue =<< seek state Forward
  V.EvKey V.KEnter [] -> Just $ M.continue =<< playSelectedSong state
  V.EvKey V.KDel [] -> Just $ M.continue =<< removeSelectedSong state
  _ -> Nothing
playlistViewEvent st ev = Nothing
