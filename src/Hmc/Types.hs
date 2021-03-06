{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-} -- just for makeLenses

module Hmc.Types
  ( stVolumeL
  , stTimeL
  , stStateL
  , stSongIDL
  , stRepeatL
  , stPlaylistLengthL
  , stSongPosL
  , WidgetName(..)
  , Event(..)
  , BrowserMode(..)
  , browserModeName
  , View(..)
  , PlaylistMode(..)
  , VolumeChange(..)
  , State(..)
  , mpdError
  , eventChannel
  , playingStatus
  , appView
  , playlistMode
  , tagsAndWidths
  , seekTimer
  , progressTimerThread
  , playlist
  , playlistTagsMaxWidths
  , currentSong
  , keyCombo
  , browserListUnderlying
  , browserList
  , traversal
  , searchInput

  , initialState
  ) where


import Protolude hiding (State)
import Lens.Micro.TH (makeLenses)
import Lens.Micro (lens, Lens')
import System.Timer.Updatable (Updatable)
import qualified Data.Map.Lazy as Map
import qualified Network.MPD as MPD
import qualified Brick.Widgets.List as L
import qualified Brick.BChan as C
import Brick.Widgets.Edit (Editor)
import Data.Vector (fromList)
import Data.Time.Clock.POSIX (POSIXTime)


-- Lenses for MPD.Status
stVolumeL :: Lens' MPD.Status (Maybe MPD.Volume)
stVolumeL = lens MPD.stVolume (\status newVolume -> status { MPD.stVolume = newVolume })


stTimeL :: Lens' MPD.Status (Maybe (MPD.FractionalSeconds, MPD.FractionalSeconds))
stTimeL = lens MPD.stTime (\status newTime -> status { MPD.stTime = newTime })


stStateL :: Lens' MPD.Status MPD.PlaybackState
stStateL = lens MPD.stState (\status newState -> status { MPD.stState = newState })


stSongIDL :: Lens' MPD.Status (Maybe MPD.Id)
stSongIDL = lens MPD.stSongID (\status newSongID -> status { MPD.stSongID = newSongID })


stRepeatL :: Lens' MPD.Status Bool
stRepeatL = lens MPD.stRepeat (\status newRepeat -> status { MPD.stRepeat = newRepeat })


stPlaylistLengthL :: Lens' MPD.Status Integer
stPlaylistLengthL = lens MPD.stPlaylistLength (\status newPlLength -> status { MPD.stPlaylistLength = newPlLength })


stSongPosL :: Lens' MPD.Status (Maybe MPD.Position)
stSongPosL = lens MPD.stSongPos (\status newSongPos -> status { MPD.stSongPos = newSongPos })


-- | Names of widgets for Brick
data WidgetName = Playlist | Browser | Search
  deriving (Eq, Ord, Show)


-- | Custom event in brick application,
-- Timer is a tick every few ms to update progress,
-- Seek is to change position in a song,
-- Change occurs when MPD.idle unblocks.
data Event = Timer | Seek | Change deriving Eq


-- | BrowserAdd and BrowserOpen display browser to add/open files to/in
-- the playlist
data BrowserMode = BrowserAdd | BrowserOpen deriving (Eq)


browserModeName :: BrowserMode -> Text
browserModeName BrowserAdd  = "Add"
browserModeName BrowserOpen = "Open"


-- | Determines which dialog is shown
data View = PlaylistView | BrowserView BrowserMode | HelpView deriving (Eq)


-- | Display songs as paths or with ID3 tags
data PlaylistMode = PlaylistPaths | PlaylistTags deriving (Eq)


-- | Isomorphic to Bool but defined and used for clarity
data VolumeChange = VolumeUp | VolumeDown


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
  , _progressTimerThread :: Maybe ThreadId

  , _playlist :: L.List WidgetName MPD.Song
  , _playlistTagsMaxWidths :: Map MPD.Metadata Int
  -- ^ Maximum length of given tag value in the current playlist,
  -- if there is no tag value for the given tag no value is in the map

  , _playlistMode :: PlaylistMode
  , _tagsAndWidths :: [(MPD.Metadata, Int)]
  -- ^ List of tags to be shown in playlist along with the maximum width
  -- of its column

  , _currentSong :: Maybe MPD.Song
  , _keyCombo :: Maybe (Char, POSIXTime)

  , _browserListUnderlying :: [MPD.LsResult]
  -- ^ The list from which browserList is made of,
  -- used as a list to be searched in by the search feature
  -- TODO: Wouldn't a Vector be a better choice here?
  , _browserList :: L.List WidgetName MPD.LsResult
  -- ^ The Brick.Widgets.List variant of the above,
  -- it differs only during search when this is limited to the searched
  -- items (while browserListUnderlying keeps the content loaded from
  -- "traversal" directory).

  , _traversal :: FilePath
  -- ^ Where is the browser in the file tree
  , _searchInput :: Maybe (Editor Text WidgetName)
  }

initialState :: C.BChan Event -> State
initialState chan = State
  { _mpdError = Just MPD.NoMPD
  , _eventChannel = chan
  , _playingStatus = MPD.def
  , _appView = PlaylistView
  , _playlistMode = PlaylistTags
  , _tagsAndWidths =
    [ (MPD.Date, 6)
    , (MPD.Artist, 25)
    , (MPD.Album, 30)
    , (MPD.Title, 120)
    ]
  , _seekTimer = Nothing
  , _progressTimerThread = Nothing
  , _playlist = L.list Playlist mempty 1
  , _playlistTagsMaxWidths = Map.empty
  , _currentSong = Nothing
  , _keyCombo = Nothing
  , _browserListUnderlying = []
  , _browserList = L.list Browser (fromList []) 1
  , _traversal = ""
  , _searchInput = Nothing
  }

makeLenses ''State
