{-# LANGUAGE OverloadedStrings #-}

module Hmc.EventHandler
  ( handleEvent
  , onStart
  , tickerInterval
  , listEndIndex
  ) where

import Protolude hiding (State)
import Unsafe (unsafeHead) -- use only when tested for non-emptiness
import Hmc.Rendering
import Hmc.Types
import System.Timer.Updatable (replacer, renewIO)
import Lens.Micro ((.~), (^.), (%~), (<&>))
import qualified Data.Text as Text
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
import qualified Brick.Widgets.Edit as E
import System.FilePath ((</>), takeDirectory, takeBaseName)
import qualified System.Directory as D
import Data.String (fromString)
import Data.List (elemIndex)


data SeekDirection = Backward | Forward


-- | Microlens doesn't have this
(%%~) = identity


-- | Extracts path from MPD's LsResult
lsResultPath :: MPD.LsResult -> FilePath
lsResultPath (MPD.LsSong p) = MPD.toString . MPD.sgFilePath $ p
lsResultPath (MPD.LsDirectory p) = MPD.toString p
lsResultPath (MPD.LsPlaylist p) = MPD.toString p


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
    tagsToLengths = Map.map (\vs -> fromMaybe 20 (Text.length . MPD.toText <$> head vs))


-- | Modify playlist and return the new playlist and state
--
-- The playlist is modified based on the given LsResult,
-- the "All Music" option is just "" dir so it works as usual.
--
-- State is modified just in case of open, not add, because new song has started
-- playing.
modifyPlaylist :: MonadIO m =>
     State -> MPD.LsResult -> m (MPD.Response ([MPD.Song], State))
modifyPlaylist st selection = liftIO $ withMPD $ do
  paths <- case selection of
      MPD.LsDirectory path -> return [path]
      MPD.LsSong song      -> return [MPD.sgFilePath song]
      MPD.LsPlaylist name  -> MPD.listPlaylist name

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
    & playlist .~ L.list Playlist (fromList playlistInfo') 1
    & playlistTagsMaxWidths .~ playlistToTagsMaxWidths (state ^. tagsAndWidths) playlistInfo'


-- | Load status and current song from MPD
loadState :: State -> MPD.MPD State
loadState state = do
  status' <- MPD.status
  currentSong' <- MPD.currentSong

  return $ state
    & playingStatus .~ status'
    & currentSong .~ currentSong'


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


-- | Returns the path of the file where traversal in browser is saved
-- between hmc runs
getTraversalFilePath :: MonadIO m => m FilePath
getTraversalFilePath = liftIO $ D.getXdgDirectory D.XdgData "hmc/traversal"


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
      savedTraversal <- liftIO (Text.unpack <$> readFile filepath)
      -- TODO: here we assume that savedTraversal is a correct path,
      -- that should be checked
      loadDirectory state (fromString savedTraversal) (const 0)


-- | On start just load playlist and state from MPD,
-- set the current song as selected in the playlist
-- and load browser position from the last time
onStart :: MonadIO m => State -> m State
onStart st = do
  st' <- runLoader st (loadState >=> loadPlaylist) >>= loadSavedTraversal
  let position = fromMaybe 0 (join $ MPD.sgIndex <$> st' ^. currentSong)
  return $ st' & playlist %~ L.listMoveTo position


-- | Saves browser traversal
onExit :: MonadIO m => State -> m ()
onExit state = do
  filepath <- getTraversalFilePath
  e <- liftIO . try $ createDirectory filepath :: MonadIO m => m (Either IOException ())
  case e of
    Left err -> return ()
    Right _  -> do
      liftIO $ writeFile filepath $ Text.pack (state ^. traversal)
      return ()
  where
    createDirectory path = D.createDirectoryIfMissing True (takeDirectory path)


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


-- | Loads the selected directory and updates traversal stack
loadDirectory :: MonadIO m => State -> MPD.Path -> ([MPD.LsResult] -> Int) -> m State
loadDirectory st dir position = do
  list <- liftIO . withMPD $ MPD.lsInfo dir
  -- In the topmost directory add "All Music" item
  let list' = if dir == ""
                then fmap addAllItem list
                else list
  return $ modifyState st updateDirContents list'
  where
    updateDirContents st contents = st
      & browserListUnderlying .~ contents
      & browserList .~
        L.listMoveTo (position contents) (L.list Browser (fromList contents) 1)
      & traversal .~ MPD.toString dir
    addAllItem = (MPD.LsDirectory "" :)


-- | Move one up in the file tree
leaveDirectory :: MonadIO m => State -> m State
leaveDirectory st =
  if thisDirectory == directoryAbove
    then return st
    else loadDirectory st (fromString directoryAbove) position
  where
    directoryAbove = directoryOneUp thisDirectory
    thisDirectory = st ^. traversal
    position list = fromMaybe 0
                  $ thisDirectory `elemIndex` map lsResultPath list


-- | Load selected item into the playlist
browserSelect :: MonadIO m => State -> (Int, MPD.LsResult) -> m State
browserSelect st (position, selection) =
  modifyState st updatePlaylist
  <$> modifyPlaylist st selection
  where
    updatePlaylist _ (list, newState) = newState
      & appView .~ PlaylistView
      & playlist .~ L.list Playlist (fromList list) 1


--- EVENT HANDLERS

-- | Handler is chosen according to the current view. If the event is
-- not specific for the view it is then passed to `commonEvent` handler
handleEvent :: State
            -> T.BrickEvent WidgetName Event
            -> T.EventM WidgetName (T.Next State)
handleEvent state event =
  case (event, state ^. searchInput) of
    (T.VtyEvent _, Just input) -> handleSearchEvent state event input
    _ -> handleViewEvent state event


handleViewEvent state = go (state ^. appView) state
  where
    go PlaylistView = playlistViewEvent
    go AddView      = browserViewEvent
    go OpenView     = browserViewEvent


runSearchLoader state search =
  runLoader state (searchLoader (state ^. appView) search)


searchLoader PlaylistView search state = do
  pl <- MPD.playlistSearch (MPD.Title MPD.=? search')
  return $ state & playlist .~ L.list Playlist (fromList pl) 1
  where search' = fromString . Text.unpack $ search


searchLoader _ search state = return $ state
  & browserList .~ L.list Browser (fromList l) 1
  where
    l = filter predicate (state ^. browserListUnderlying)
    predicate = match search . fromString . lsResultPath


leaveSearch PlaylistView state = do
  state' <- runSearchLoader state ""
  let chosenSong = fromMaybe 0 (join $ MPD.sgIndex <$> state' ^. currentSong)
  return $ state'
    & playlist %~ L.listMoveTo chosenSong
    & searchInput .~ Nothing


leaveSearch _ state = return $ state
  & browserList .~ L.list Browser v 1
  & searchInput .~ Nothing
  where v = fromList (state ^. browserListUnderlying)


handleSearchEvent :: State
                  -> T.BrickEvent WidgetName Event
                  -> E.Editor Text WidgetName
                  -> T.EventM WidgetName (T.Next State)
handleSearchEvent state e@(T.VtyEvent event) input =
  case event of
    V.EvKey V.KEsc [] -> M.continue =<< leaveSearch (state ^. appView) state
    V.EvKey V.KRight [] ->
      case state ^. appView of
        PlaylistView -> handleViewEvent state e
        _            -> M.continue
                    =<< leaveSearch (state ^. appView)
                    =<< enterDirectory state
    V.EvKey V.KUp [] -> handleViewEvent state e
    V.EvKey V.KDown [] -> handleViewEvent state e
    V.EvKey V.KEnter [] -> handleViewEvent state e
    _ -> M.continue =<< do
      input' <- E.handleEditorEvent event input
      let stateNewInput = state & searchInput .~ Just input'
      let search = fromMaybe "" . head $ E.getEditContents input'
      runSearchLoader stateNewInput search


-- | Matches against the files' base name, case insensitive, looks for
-- match anywhere
-- TODO: Get rid of the unpack and pack to speed it up.
match needle haystack = Text.toLower needle `Text.isInfixOf` Text.toLower base
  where base = Text.pack . takeBaseName . Text.unpack $ haystack


playlistViewEvent :: State
                  -> T.BrickEvent WidgetName Event
                  -> T.EventM WidgetName (T.Next State)
playlistViewEvent st (T.VtyEvent e) = case e of
  V.EvKey (V.KChar 'g') [] -> M.continue =<< handleKeyCombo 'g' 'g' move st
    where move st = return $ st & playlist %~ L.listMoveTo 0
  V.EvKey V.KLeft [] -> M.continue =<< seek st Backward
  V.EvKey V.KRight [] -> M.continue =<< seek st Forward
  V.EvKey V.KEnter [] ->
    M.continue =<< maybe (return st) (\(_, song) -> play st song) selection
    where
      selection :: Maybe (Int, MPD.Song)
      selection = L.listSelectedElement $ st ^. playlist

  _ -> commonEvent st (T.VtyEvent e)
playlistViewEvent st ev = commonEvent st ev


-- | Enters the directory chosen in browser.
-- If there's nothing chosen does nothing,
-- if something else than a directory is chosen, does nothing.
enterDirectory state = maybe (return state) enter selection
  where
    selection :: Maybe MPD.LsResult
    selection = snd <$> L.listSelectedElement (state ^. browserList)
    enter (MPD.LsDirectory dir) = loadDirectory state dir (const 0)
    enter _                     = return state


browserViewEvent :: State
                 -> T.BrickEvent WidgetName Event
                 -> T.EventM WidgetName (T.Next State)
browserViewEvent st (T.VtyEvent e) = case e of
  V.EvKey (V.KChar 'g') [] -> M.continue =<< handleKeyCombo 'g' 'g' move st
    where move st = return $ st & browserList %~ L.listMoveTo 0
  V.EvKey V.KLeft [] -> M.continue =<< leaveDirectory st
  V.EvKey V.KRight [] -> M.continue =<< enterDirectory st
  V.EvKey V.KEnter [] ->
    M.continue =<< maybe (return st) (browserSelect st) selection
    where selection = L.listSelectedElement $ st ^. browserList

  _ -> commonEvent st (T.VtyEvent e)
browserViewEvent st ev = commonEvent st ev


commonEvent :: State
            -> T.BrickEvent WidgetName Event
            -> T.EventM WidgetName (T.Next State)
commonEvent st (T.VtyEvent e) = case e of
  V.EvKey (V.KChar 'q') [] -> onExit st >> M.halt st
  V.EvKey (V.KChar 'd') [V.MCtrl] -> onExit st >> M.halt st
  V.EvKey (V.KChar '\t') [] -> M.continue =<< playNext st
  V.EvKey (V.KFun 2) [] -> M.continue $ st & appView .~ PlaylistView
  V.EvKey (V.KFun 3) [] -> M.continue $ st & appView .~ AddView
  V.EvKey (V.KFun 4) [] -> M.continue $ st & appView .~ OpenView
  V.EvKey (V.KFun 5) [] -> M.continue =<< runLoader st loader
    where loader st' = MPD.update Nothing
                       >> MPD.idle [MPD.DatabaseS]
                       >> loadState st'

  V.EvKey (V.KChar '/') [] -> M.continue $ st
    & searchInput .~ Just (E.editorText Search renderSearchContent (Just 1) "")
  V.EvKey V.KEsc []     -> M.continue $
    case st ^. searchInput of
      Nothing -> st & appView .~ PlaylistView
      Just _  -> st & searchInput .~ Nothing

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

  V.EvKey V.KUp [] -> moveListAndContinue
  V.EvKey V.KDown [] -> moveListAndContinue
  V.EvKey V.KUp [V.MShift] -> moveListAndContinue
  V.EvKey V.KDown [V.MShift] -> moveListAndContinue
  V.EvKey (V.KChar 'f') [V.MCtrl] -> moveListAndContinue
  V.EvKey (V.KChar 'b') [V.MCtrl] -> moveListAndContinue
  V.EvKey V.KPageUp [] -> moveListAndContinue
  V.EvKey V.KPageDown [] -> moveListAndContinue
  V.EvKey (V.KChar 'G') [] -> moveListAndContinue

  _ -> M.continue st

  where
    moveListAndContinue = M.continue =<< goListMovement (st ^. appView)
    goListMovement PlaylistView = st & playlist %%~ handleListMovement e
    goListMovement AddView      = st & browserList %%~ handleListMovement e
    goListMovement OpenView     = st & browserList %%~ handleListMovement e

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


-- | Handles common list movement events and returns the list (in the event monad)
-- Key combos are not handled here as they also modify the state.
handleListMovement :: Ord n => V.Event -> L.List n e -> T.EventM n (L.List n e)
handleListMovement event list =
  case event of
    V.EvKey (V.KChar 'G') [] -> return $ L.listMoveTo (listEndIndex list) list
    V.EvKey (V.KChar 'f') [V.MCtrl] -> L.handleListEvent (V.EvKey V.KPageDown []) list
    V.EvKey (V.KChar 'b') [V.MCtrl] -> L.handleListEvent (V.EvKey V.KPageUp []) list
    V.EvKey V.KUp [V.MShift]   -> return $ maybe list move5up   (list ^. L.listSelectedL)
    V.EvKey V.KDown [V.MShift] -> return $ maybe list move5down (list ^. L.listSelectedL)
    _ -> L.handleListEvent event list -- Brick's default handler

    where
      move5up   pos = L.listMoveTo (max 0 (pos - 5)) list
      move5down pos = L.listMoveTo (       pos + 5 ) list
