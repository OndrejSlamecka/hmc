{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude hiding (State, state, list, length)
import Hmc.Common
import Hmc.Types
import Hmc.Playlist
import Hmc.Browser
import Hmc.Search
import Control.Concurrent (threadDelay, forkIO)
import Lens.Micro ((.~), (^.), (%~), (<&>))
import qualified Data.Text as Text (length, pack, null, take, drop, stripStart)
import qualified Network.MPD as MPD
import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Util as U
import qualified Brick.BChan as C
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Center as Center
import Brick.Widgets.Border (hBorderWithLabel)
import Brick.Widgets.Core
  ( txt
  , vBox
  , hBox
  )


main :: IO ()
main = do
  chan <- C.newBChan 10
  _ <- forkIO . forever $ do
    C.writeBChan chan Timer
    threadDelay tickerInterval

  _ <- forkIO . forever $ do
    _ <- MPD.withMPD $ MPD.idle [MPD.DatabaseS]
    C.writeBChan chan Change

  void $ M.customMain (V.mkVty V.defaultConfig) (Just chan) theApp (initialState chan)


theApp :: M.App State Event WidgetName
theApp = M.App
  { M.appDraw = drawUI
  , M.appHandleEvent = handleEvent
  , M.appChooseCursor = M.showFirstCursor
  , M.appStartEvent = onStart
  , M.appAttrMap = const attributeMap
  }


attributeMap :: A.AttrMap
attributeMap = A.attrMap V.defAttr
  [ (L.listSelectedAttr, V.blue `U.on` V.white)
  , (playingSongAttr   , V.withStyle V.currentAttr V.bold)
  ]


--- Event Handling ------------------------------------------------------------


-- | Every tickerInterval microseconds a Timer event is sent and the
-- playing progress is updated (if the player is playing).
-- The thread sending this event is created in the main file
tickerInterval :: Int
tickerInterval = 128*1000


-- | Modify progress of playing of the current song and
-- load new song from MPD if needed.
--
-- This function is not a loader (for runLoader) as running it with
-- runLoader would discard any temporary error (e.g. non-existing
-- directory) with each tick (making it almost invisible to the user).
--
-- runLoader is invoked inside (thus possibly discarding errors) when a
-- song ends but that seems reasonably rare (if this is causing you
-- problems create a github issue).
progress :: MonadIO m => State -> Double -> m State
progress st t = do
  case MPD.stTime (st ^. playingStatus) of
    Nothing   -> return st
    Just time ->
      -- Reload state if a song just started playing, otherwise just
      -- increase time count
      if fst incrementedTime > fromIntegral (snd time)
        then runLoader st loadState
        else return $ st & playingStatus . stTimeL .~ Just incrementedTime

      where
        incrementedTime = if MPD.stState (st ^. playingStatus) == MPD.Playing
          then (fst time + t, snd time)
          else time


-- | On start just load playlist and state from MPD,
-- set the current song as selected in the playlist
-- and load browser position from the last time
onStart :: MonadIO m => State -> m State
onStart st = do
  st' <- runLoader (st & mpdError .~ Nothing) (loadState >=> loadPlaylist) >>= loadSavedTraversal
  let position = fromMaybe 0 (join $ MPD.sgIndex <$> st' ^. currentSong)
  return $ st' & playlist %~ L.listMoveTo position


-- | Saves browser traversal
onExit :: MonadIO m => State -> m ()
onExit = saveTraversal


-- | Toggles playing status
togglePlay :: MonadIO m => State -> m State
togglePlay state =
  runAction state toggle
  >> return (state & playingStatus . stStateL %~ flipState)
  where
    toggle :: State -> MPD.MPD ()
    toggle state' = case MPD.stState (state' ^. playingStatus) of
                      MPD.Playing -> MPD.pause True
                      MPD.Stopped -> MPD.play Nothing
                      MPD.Paused  -> MPD.play Nothing
    -- | Note that Stopped is avoided
    flipState plState = case plState of
      MPD.Playing -> MPD.Paused
      MPD.Stopped -> MPD.Playing
      MPD.Paused  -> MPD.Playing


-- | Pauses MPD if it is playing
pause :: MonadIO m => State -> m ()
pause state = when playing doPause
  where
    playing = MPD.stState (state ^. playingStatus) == MPD.Playing
    doPause = void . liftIO . MPD.withMPD $ MPD.pause True


-- Opens the search dialog if the view supports it
openSearch :: State -> State
openSearch st = go (st ^. appView)
  where
    go PlaylistView    = st & searchInput .~ Just createSearchInput
    go (BrowserView _) = st & searchInput .~ Just createSearchInput
    go _               = st


-- | If search is open and can handle the event it is handled by
-- handleSearchEvent. Otherwise handleViewEvent is used (which might
-- then use commonEvent).
handleEvent :: State
            -> T.BrickEvent WidgetName Event
            -> T.EventM WidgetName (T.Next State)
handleEvent state event =
  case (event, state ^. searchInput) of
    (T.VtyEvent _, Just input) ->
      fromMaybe (handleViewEvent state event) (handleSearchEvent state event input)
    _ -> handleViewEvent state event


-- | Handler is chosen according to the current view. If the event is
-- not specific for the view it is then passed to `commonEvent` handler
handleViewEvent :: State
                -> T.BrickEvent WidgetName Event
                -> T.EventM WidgetName (T.Next State)
handleViewEvent state event =
  fromMaybe (commonEvent state event)
    (go (state ^. appView) state event)
  where
    go PlaylistView    = playlistViewEvent
    go (BrowserView _) = browserViewEvent
    go HelpView        = const (const Nothing)


-- | Handler of events common for all views and situations.
commonEvent :: State
            -> T.BrickEvent WidgetName Event
            -> T.EventM WidgetName (T.Next State)
commonEvent st (T.VtyEvent e) = case e of
  V.EvKey (V.KChar 'q') [V.MCtrl] -> onExit st >> M.halt st
  V.EvKey (V.KChar 'q') [] -> pause st >> onExit st >> M.halt st
  V.EvKey (V.KChar 'd') [V.MCtrl] -> pause st >> onExit st >> M.halt st
  V.EvKey (V.KChar '\t') [] -> M.continue =<< playNext st
  V.EvKey (V.KFun 1) []  -> M.continue $ st & appView .~ HelpView
  V.EvKey (V.KFun 12) [] -> M.continue $ st & appView .~ HelpView
  V.EvKey (V.KFun 2) []  -> M.continue $ st & appView .~ PlaylistView
  V.EvKey (V.KFun 3) []  -> M.continue $ st & appView .~ BrowserView BrowserAdd
  V.EvKey (V.KFun 4) []  -> M.continue $ st & appView .~ BrowserView BrowserOpen
  V.EvKey (V.KFun 5) []  -> M.continue =<< runAction st (return $ MPD.update Nothing)
  V.EvKey (V.KChar '/') [] -> M.continue $ openSearch st
  V.EvKey (V.KChar 's') [] -> M.continue $ openSearch st
  V.EvKey V.KEsc []     -> M.continue $
    case st ^. searchInput of
      Nothing -> st & appView .~ PlaylistView
      Just _  -> st & searchInput .~ Nothing

  V.EvKey (V.KChar ' ') [] ->
    case st ^. mpdError of
      -- When file is not found update MPD and start over from the base directory
      Just (MPD.ACK MPD.FileNotFound _) ->
            runAction st (return $ MPD.update Nothing)
         >> reloadDirectory (st & mpdError .~ Nothing & traversal .~ "")
        >>= M.continue
      -- In case of other errors try to start over
      Just _ -> M.continue =<< onStart (st & mpdError .~ Nothing)
      -- otherwise (un)pause
      Nothing -> M.continue =<< togglePlay st

  V.EvKey V.KUp []                -> moveListAndContinue
  V.EvKey V.KDown []              -> moveListAndContinue
  V.EvKey V.KUp [V.MShift]        -> moveListAndContinue
  V.EvKey V.KDown [V.MShift]      -> moveListAndContinue
  V.EvKey (V.KChar 'f') [V.MCtrl] -> moveListAndContinue
  V.EvKey (V.KChar 'b') [V.MCtrl] -> moveListAndContinue
  V.EvKey V.KPageUp []            -> moveListAndContinue
  V.EvKey V.KPageDown []          -> moveListAndContinue
  V.EvKey (V.KChar 'G') []        -> moveListAndContinue

  _ -> M.continue st

  where
    moveListAndContinue = M.continue =<< goListMovement (st ^. appView)
    goListMovement PlaylistView    = st & playlist %%~ handleListMovement e
    goListMovement (BrowserView _) = st & browserList %%~ handleListMovement e
    goListMovement HelpView        = return st

commonEvent st (T.AppEvent Timer) = case MPD.stTime (st ^. playingStatus) of
  Nothing   -> M.continue st -- This happening means a bug or a concurrency problem
  Just _    -> M.continue
    =<< progress st tickerIntervalInMs
    where tickerIntervalInMs = fromIntegral tickerInterval / (1000*1000)

commonEvent st (T.AppEvent Seek) = M.continue =<<
  case join $ MPD.sgId <$> st ^. currentSong of
    Nothing -> return st
    Just id -> runLoader st (loader id) <&> seekTimer .~ Nothing
  where
    loader id st' = MPD.seekId id (round . fst $ time) >> return st'
    time = fromMaybe (0,0) (st ^. playingStatus . stTimeL)

commonEvent st (T.AppEvent Change) = M.continue =<< reloadDirectory st
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


--- Rendering -----------------------------------------------------------------


--
-- Utility functions
--

stateToText :: MPD.State -> Text
stateToText state = case state of
  MPD.Stopped -> "■"
  MPD.Paused  -> "⏸" -- http://www.fileformat.info/info/unicode/char/23f8/index.htm
  MPD.Playing -> "▶"


-- | Wraps the given text in a single space on both sides
spacepad :: (IsString a, Semigroup a) => a -> a
spacepad s = " " <> s <> " "


-- | Formats seconds into timer format, e.g. 67 -> 1:07
timerFormat :: Integer -> Text
timerFormat s = minutes s <> ":" <> seconds s
  where
    minutes t = i2t . (floor :: Double -> Integer) $ (fromIntegral t / 60 :: Double)
    seconds t = leadingZero . i2t $ t `mod` 60
    leadingZero :: Text -> Text
    leadingZero t = if Text.length t < 2 then "0" <> t else t
    i2t :: Integral a => a -> Text
    i2t = Text.pack . show . toInteger


-- | Main rendering function
drawUI :: State -> [T.Widget WidgetName]
drawUI appState = case appState ^. mpdError of
  Just err ->
    [ Center.vCenter $ vBox $ map (Center.hCenter . txt)
      [ show err
      , "Spacebar to restart, q to quit"
      ]
    ]
  Nothing -> [ vBox body ]

  where
    body = case appState ^. searchInput of
      Nothing ->
        case appState ^. appView of
          PlaylistView  -> view ++ rprogress
          BrowserView _ -> view ++ rprogress
          _             -> view
      Just input -> view ++ rprogress ++ search input
    view         = [ renderView appState ]
    rprogress    = [ hBorderWithLabel $ renderProgress appState ]
    search input = [ hBox [ txt "/", E.renderEditor True input ] ]


--
-- Widgets
--

renderProgress :: State -> T.Widget n
renderProgress appState = txt $ spacepad (elapsedT <> "/" <> totalT)
  where
    elapsedT = timerFormat . floor . fst $ time
    totalT = timerFormat . snd $ time
    time = fromMaybe (0,0) (appState ^. playingStatus . stTimeL)


renderView :: State -> T.Widget WidgetName
renderView appState = case appState ^. appView of
  PlaylistView -> renderPlaylist appState
  BrowserView mode -> renderBrowser (browserModeName mode) appState
  HelpView -> renderHelp


renderHelp :: T.Widget WidgetName
renderHelp = vBox
  [ txt "hmc by Ondrej Slamecka\n"
  , txt "https://github.com/ondrejslamecka/hmc\n"
  , txt "This program is provided under the 3-clause BSD license.\n"
  , txt "\n"
  , txt "Controls"
  , txt "\n"
  , txt $ wrap 80 "There are three views: Help (F1, F12), Playlist (F2, Esc), Browser (F3 adding   mode, F4 opening mode).\n"
  , txt $ wrap 80 "You can exit with q or Ctrl-d, if you want to keep the music playing then exit  with Ctrl-q.\n"
  , txt $ wrap 80 "In lists move up and down with arrows, press enter to play/(add/open), spacebar to (un)pause, Tab to play the next song.\n"
  , txt $ wrap 80 "In playlist left and right arrows are used to seek in the song, in browser to enter/leave directory.\n"
  , txt $ wrap 80 "You can use gg, G, Ctrl-f or PageDown, Ctrl-b or PageUp, Shift-up, Shift-down to move faster in lists.\n"
  , txt "Search with / or s and leave search with Esc."
  ]


wrap :: Int -> Text -> Text
wrap n t = go t ""
  where
    go :: Text -> Text -> Text
    go s a
      | Text.null s = a
      | otherwise   = go (rest s) (a <> newline a <> Text.take n s)
    newline a = if Text.null a then "" else "\n"
    rest = Text.stripStart . Text.drop n
