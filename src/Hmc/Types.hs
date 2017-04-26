{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-} -- just for makeLenses

-- TODO: rename State/State, View/View, Event/Event
module Hmc.Types
  ( State(..)
  , mpdError
  , eventChannel
  , playingStatus
  , appView
  , seekTimer
  , playlist
  , currentSong
  , keyCombo
  , currentDirContents
  , traversal
  , initialState
  , View(..)
  , Event(..)
  , stTimeL
  , stStateL
  , stSongIDL
  ) where

import Protolude hiding (State)
import Lens.Micro.TH (makeLenses)
import Lens.Micro (lens, Lens')
import System.Timer.Updatable (Updatable)
import qualified Network.MPD as MPD
import qualified Brick.Widgets.List as L
import qualified Brick.BChan as C
import Data.Vector (fromList)
import Data.Time.Clock.POSIX (POSIXTime)


-- Lenses for MPD.Status
stTimeL :: Lens' MPD.Status (Maybe (Double, MPD.Seconds))
stTimeL = lens MPD.stTime (\status newTime -> status { MPD.stTime = newTime })

stStateL :: Lens' MPD.Status MPD.State
stStateL = lens MPD.stState (\status newState -> status { MPD.stState = newState })

stSongIDL :: Lens' MPD.Status (Maybe MPD.Id)
stSongIDL = lens MPD.stSongID (\status newSongID -> status { MPD.stSongID = newSongID })


-- | Custom event in brick application
data Event = Timer | Seek


-- | Determines which dialog is shown
--
-- AddView and OpenView display browser to add/open files to/in the
-- playlist
data View = PlaylistView | AddView | OpenView deriving (Eq)


-- | Stores the path the user traversed in the file browser.
type TraversalStack = [(Int, MPD.Path)]


-- | Brick application state
--
-- The ID of current song is stored twice:
-- once in playingStatus and once in currentSong,
-- but we (may) need the extra information of both
data State = State
  { _mpdError :: Maybe MPD.MPDError
  , _eventChannel :: C.BChan Event
  , _playingStatus :: MPD.Status
  , _appView :: View
  , _seekTimer :: Maybe (Updatable ())

  , _playlist :: L.List () MPD.Song
  , _currentSong :: Maybe MPD.Song
  , _keyCombo :: Maybe (Char, POSIXTime)

  , _currentDirContents :: L.List () (Maybe MPD.LsResult)
  -- ^ If an item is Nothing then it represents the "all music" option
  -- The current implementation places this "all music" list above the
  -- file tree.
  , _traversal :: TraversalStack
  }

initialState :: C.BChan Event -> State
initialState chan = State
  { _mpdError = Just MPD.NoMPD
  , _eventChannel = chan
  , _playingStatus = MPD.def
  , _appView = PlaylistView
  , _seekTimer = Nothing
  , _playlist = L.list () mempty 1
  , _currentSong = Nothing
  , _keyCombo = Nothing
  , _currentDirContents = L.list () (fromList [Nothing]) 1
  , _traversal = []
  }

makeLenses ''State
