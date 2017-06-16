{-# LANGUAGE OverloadedStrings #-}

module Hmc.EventHandler
  ( handleEvent
  , onStart
  , tickerInterval
  ) where

import Protolude hiding (State)
import Hmc.Rendering
import Hmc.Types
import System.Timer.Updatable (replacer, renewIO)
import Lens.Micro ((.~), (^.), (%~), (<&>))
import qualified Data.Text as Text (length)
import qualified Network.MPD as MPD
import Network.MPD (withMPD)
import Data.Vector (fromList)
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as V (length)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.BChan as C
import qualified Brick.Types as T


data SeekDirection = Backward | Forward


-- | Every tickerInterval microseconds a Timer event is sent and the
-- playing progress is updated (if the player is playing).
-- The thread sending this event is created in the main file
tickerInterval :: Int
tickerInterval = 128*1000


-- | Returns the position of the current song in the playlist or 0 if
-- there is no such song
currentSongPosition st = fromMaybe 0 maybePos
  where maybePos = join $ MPD.sgIndex <$> st ^. currentSong


-- | Assigns opposite states, avoiding Stopped
flipState :: MPD.State -> MPD.State
flipState state = case state of
  MPD.Playing -> MPD.Paused
  MPD.Stopped -> MPD.Playing
  MPD.Paused  -> MPD.Playing


-- | Returns the last index in the given list (in O(1) time)
listEndIndex :: L.List n e -> Int
listEndIndex list = V.length (list ^. L.listElementsL) - 1


-- | Takes a playlist and returns a map from tag to maximum length of
-- its value (or Nothing if there is no value)
playlistToTagsMaxWidths :: [(MPD.Metadata, Int)] -> [MPD.Song] -> Map MPD.Metadata Int
playlistToTagsMaxWidths tagsAndWidths playlist = fromMaybe Map.empty
  $ foldr1May (Map.unionWith max) tagLengthMaps
  where
    tagLengthMaps = map (tagsToLengths . MPD.sgTags) playlist
    -- If the list of values is empty the length is set to 20 to leave
    -- some space for an error text (this approach should be improved)
    tagsToLengths = Map.map (\vs -> fromMaybe 20 (Text.length <$> MPD.toText <$> head vs))


-- | Modify playlist and return the new playlist and state
--
-- The playlist is modified based on the given Maybe LsResult, in case
-- of `Nothing` it adds all music.
--
-- State is modified just in case of open, not add, because new song has started
-- playing.
modifyPlaylist :: MonadIO m =>
     State -> Maybe MPD.LsResult -> m (MPD.Response ([MPD.Song], State))
modifyPlaylist st selection = liftIO $ withMPD $ do
  paths <- case selection of
      Nothing -> return [""]
      Just (MPD.LsDirectory path) -> return [path]
      Just (MPD.LsSong song) -> return [MPD.sgFilePath song]
      Just (MPD.LsPlaylist name) -> MPD.listPlaylist name

  when (st ^. appView == OpenView) (void MPD.clear)
  void $ forM_ paths MPD.add
  when (st ^. appView == OpenView) (MPD.play Nothing)

  plInfo <- MPD.playlistInfo Nothing

  newState <- if st ^. appView == OpenView
    then loadPlaylist st >>= loadState
    else return st

  return (plInfo, newState)


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


-- STATE MANAGEMENT

-- | In case MPD result is an error, returns the state with error,
-- otherwise returns the state after applying a modifier which gets
-- passed the state and result contents
modifyState :: State -> (State -> a -> State) -> MPD.Response a -> State
modifyState state modifier either = case either of
  Left err -> state & mpdError .~ Just err
  Right result -> modifier state result & mpdError .~ Nothing


-- | Returns either the old state with mpdError updated or the new state
-- with mpdError emptied
stateFromEither :: State -> MPD.Response State -> State
stateFromEither oldSt = modifyState oldSt (\oldSt' newSt -> newSt)


-- | Load information using given loader from MPD, run the MPD monad and
-- extract the contents of the response (i.e. add error to old state on
-- error, or replace it with the new state)
runLoader :: MonadIO m =>
  State -> (State -> MPD.MPD State) -> m State
runLoader st loader = stateFromEither st <$> liftIO (withMPD $ loader st)


-- | Runs an MPD action and updates the mpdError in app state
runAction :: MonadIO m =>
  State -> MPD.MPD State -> m State
runAction st loader = runLoader st (\st' -> loader >> return st')


-- LOADERS
-- Functions modifying state in the MPD monad

-- | Load playlist information from MPD
loadPlaylist :: State -> MPD.MPD State
loadPlaylist state = do
  playlistInfo' <- MPD.playlistInfo Nothing

  return $ state
    & playlist .~ L.list () (fromList playlistInfo') 1
    & playlistTagsMaxWidths .~ playlistToTagsMaxWidths (state ^. tagsAndWidths) playlistInfo'


-- | Load status and current song from MPD
loadState :: State -> MPD.MPD State
loadState state = do
  status' <- MPD.status
  currentSong' <- MPD.currentSong

  return $ state
    & playingStatus .~ status'
    & currentSong .~ currentSong'


loadDirectory state = do
  dirContents' <- case MPD.lsInfo . snd <$> headMay (state ^. traversal) of
    Nothing -> return [Nothing]
    Just list -> map Just <$> list -- list is in MPD, thus <$>

  return $ state
    & currentDirContents .~ L.list () (fromList dirContents') 1


-- | Modify progress of playing of current song and
-- load new song from MPD if needed
progressLoader :: Double -> State -> MPD.MPD State
progressLoader t st =
  case MPD.stTime (st ^. playingStatus) of
    Nothing   -> return st
    Just time ->
      -- Reload state if a song just started playing, otherwise just
      -- increase time count
      if fst incrementedTime > fromIntegral (snd time)
        then loadState st
        else return $ st & playingStatus . stTimeL .~ Just incrementedTime

      where
        incrementedTime = if MPD.stState (st ^. playingStatus) == MPD.Playing
          then (fst time + t, snd time)
          else time


--
-- ACTIONS
--

-- | On start just load playlist and state from MPD
-- and set the current song as select in the playlist
onStart :: MonadIO m => State -> m State
onStart st = do
  st' <- runLoader st (loadState >=> loadPlaylist)
  let position = fromMaybe 0 (join $ MPD.sgIndex <$> st' ^. currentSong)
  return $ st' & playlist %~ L.listMoveTo position


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


--- Browser action


-- | Proceed to the selected directory
enterDirectory :: MonadIO m => State -> (Int, MPD.Path) -> m State
enterDirectory st (pos, dir) =
  modifyState st updateDirContents
  <$> liftIO (withMPD $ map Just <$> MPD.lsInfo dir)
  where
    traversal' = (pos, dir):(st ^. traversal)
    updateDirContents st contents = st
      & currentDirContents .~ L.list () (fromList contents) 1
      & traversal .~ traversal'


-- | Move one up in the file tree
-- If there is nothing above this position, then create a dummy
-- "All Music" list (represented with [Nothing] here)
leaveDirectory :: MonadIO m => State -> m State
leaveDirectory st =
  modifyState st updateContents
  <$> case directoryAbove of
        Nothing -> return $ return [Nothing]
        Just dir -> liftIO (withMPD $ map Just <$> MPD.lsInfo dir)
  where
    directoryAbove = atMay (snd <$> st ^. traversal) 1
    position = headDef 0 (fst <$> st ^. traversal)
    updateContents st' contents = st'
      & currentDirContents .~
        L.listMoveTo position (L.list () (fromList contents) 1)
      & traversal .~ fromMaybe [] (tailMay (st ^. traversal))


-- | Load selected item into the playlist
browserSelect :: MonadIO m => State -> (Int, Maybe MPD.LsResult) -> m State
browserSelect st (position, selection) =
  modifyState st updatePlaylist
  <$> modifyPlaylist st selection
  where
    updatePlaylist _ (list, newState) = newState
      & appView .~ PlaylistView
      & playlist .~ L.list () (fromList list) 1


--- EVENT HANDLERS

-- | Event handler is first chosen according to current view, if the
-- chosen handler does not catch the event it passes it to the
-- commonEvent handler

handleEvent :: State -> T.BrickEvent () Event -> T.EventM () (T.Next State)
handleEvent st = go (st ^. appView) st
  where
    go PlaylistView = playlistViewEvent
    go AddView      = browserViewEvent
    go OpenView     = browserViewEvent


playlistViewEvent :: State -> T.BrickEvent () Event -> T.EventM () (T.Next State)
playlistViewEvent st (T.VtyEvent e) = case e of
  V.EvKey (V.KChar 'G') [] -> M.continue $
    st & playlist %~ L.listMoveTo (listEndIndex $ st ^. playlist)
  V.EvKey (V.KChar 'g') [] -> M.continue =<< handleKeyCombo 'g' 'g' move st
    where move st = return $ st & playlist %~ L.listMoveTo 0
  V.EvKey V.KUp [] -> M.continue $ st & playlist %~ L.listMoveUp
  V.EvKey V.KDown [] -> M.continue $ st & playlist %~ L.listMoveDown
  V.EvKey V.KLeft [] -> M.continue =<< seek st Backward
  V.EvKey V.KRight [] -> M.continue =<< seek st Forward
  V.EvKey V.KEnter [] ->
    M.continue =<< maybe (return st) (\(_, song) -> play st song) selection
    where
      selection :: Maybe (Int, MPD.Song)
      selection = L.listSelectedElement $ st ^. playlist

  _ -> commonEvent st (T.VtyEvent e)
playlistViewEvent st ev = commonEvent st ev


browserViewEvent :: State -> T.BrickEvent () Event -> T.EventM () (T.Next State)
browserViewEvent st (T.VtyEvent e) = case e of
  V.EvKey (V.KChar 'G') [] -> M.continue $
    st & currentDirContents %~ L.listMoveTo (listEndIndex $ st ^. currentDirContents)
  V.EvKey (V.KChar 'g') [] -> M.continue =<< handleKeyCombo 'g' 'g' move st
    where move st = return $ st & currentDirContents %~ L.listMoveTo 0
  V.EvKey V.KUp [] -> M.continue $ st & currentDirContents %~ L.listMoveUp
  V.EvKey V.KDown [] -> M.continue $ st & currentDirContents %~ L.listMoveDown
  V.EvKey V.KLeft [] -> M.continue =<< leaveDirectory st
  V.EvKey V.KRight [] -> M.continue =<< maybe (return st) enter selection
    where
      selection :: Maybe (Int, Maybe MPD.LsResult)
      selection = L.listSelectedElement $ st ^. currentDirContents
      enter (pos, Just (MPD.LsDirectory dir)) = enterDirectory st (pos, dir)
      enter (pos, Nothing)                    = enterDirectory st (0, "")
      enter _                                 = return st

  V.EvKey V.KEnter [] ->
    M.continue =<< maybe (return st) (browserSelect st) selection
    where selection = L.listSelectedElement $ st ^. currentDirContents

  _ -> commonEvent st (T.VtyEvent e)
browserViewEvent st ev = commonEvent st ev


commonEvent :: State -> T.BrickEvent () Event -> T.EventM () (T.Next State)
commonEvent st (T.VtyEvent e) = case e of
  V.EvKey (V.KChar 'q') [] -> M.halt st
  V.EvKey (V.KChar 'd') [V.MCtrl] -> M.halt st
  V.EvKey (V.KChar '\t') [] -> M.continue =<< playNext st
  V.EvKey (V.KFun 2) [] -> M.continue $ st & appView .~ PlaylistView
  V.EvKey V.KEsc []     -> M.continue $ st & appView .~ PlaylistView
  V.EvKey (V.KFun 3) [] -> M.continue $ st & appView .~ AddView
  V.EvKey (V.KFun 4) [] -> M.continue $ st & appView .~ OpenView
  V.EvKey (V.KFun 5) [] -> M.continue =<< runLoader st loader
    where loader st' = MPD.update Nothing
                       >> MPD.idle [MPD.DatabaseS]
                       >> loadState st'

  V.EvKey (V.KChar ' ') [] ->
    case st ^. mpdError of
      -- attempt reload when in error state
      Just error -> do
        st' <- runLoader st loadState
        M.continue st'

      -- otherwise (un)pause
      Nothing -> do
        liftIO $ withMPD $ case MPD.stState $ st ^. playingStatus of
          MPD.Playing -> MPD.pause True
          MPD.Stopped -> MPD.play Nothing
          MPD.Paused  -> MPD.play Nothing

        M.continue $ st & playingStatus . stStateL %~ flipState

  _ -> M.continue st
commonEvent st (T.AppEvent Timer) = case MPD.stTime (st ^. playingStatus) of
  Nothing   -> M.continue st -- This happening means a bug or a concurrency problem
  Just time -> M.continue
    =<< runLoader st (progressLoader tickerIntervalInMs)
    where tickerIntervalInMs = fromIntegral tickerInterval / (1000*1000)

commonEvent st (T.AppEvent Seek) = M.continue =<<
  case join $ MPD.sgId <$> st ^. currentSong of
    Nothing -> return st
    Just id -> runLoader st (loader id) <&> seekTimer .~ Nothing
  where
    loader id st' = MPD.seekId id (round . fst $ time) >> return st'
    time = fromMaybe (0,0) (st ^. playingStatus . stTimeL)
commonEvent st _ = M.continue st
