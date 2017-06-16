{-# LANGUAGE OverloadedStrings #-}

module Hmc.Rendering
  ( drawUI
  , playingSongAttr
  ) where

import Hmc.Types
import Protolude hiding (State, length)
import Lens.Micro ((.~), (^.), (%~))
import Data.Text (length, pack, take)
import qualified Data.Map.Lazy as Map (lookup)
import qualified Network.MPD as MPD
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.List as L
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Util as U
import qualified Brick.BChan as C
import qualified Brick.Widgets.Center as Center
import Brick.Widgets.Border (hBorder, hBorderWithLabel)
import Brick.Widgets.Core
  ( txt
  , vBox
  , hBox
  , withAttr
  , padRight
  , hLimit
  )

--
-- Attributes names
--

-- | Attribute for playing song in list
playingSongAttr :: A.AttrName
playingSongAttr = "playingSongAttr"


--
-- Utility functions
--

stateToText :: MPD.State -> Text
stateToText state = case state of
  MPD.Stopped -> "■"
  MPD.Paused  -> "⏸" -- http://www.fileformat.info/info/unicode/char/23f8/index.htm
  MPD.Playing -> "▶"


-- | Wraps the given text in a single space on both sides
spacepad s = " " <> s <> " "


-- | Formats seconds into timer format, e.g. 67 -> 1:07
timerFormat :: Integral a => a -> Text
timerFormat s = minutes s <> ":" <> seconds s
  where
    minutes :: Integral a => a -> Text
    minutes s = i2t . floor $ fromIntegral s / 60
    seconds :: Integral a => a -> Text
    seconds s = leadingZero . i2t $ s `mod` 60
    leadingZero :: Text -> Text
    leadingZero t = if length t < 2 then "0" <> t else t
    i2t :: Integral a => a -> Text
    i2t = pack . show . toInteger


-- | Main rendering function
drawUI :: State -> [T.Widget ()]
drawUI appState = case appState ^. mpdError of
  Just error ->
    [ Center.vCenter $ vBox $ map (Center.hCenter . txt)
      [ show error
      , "Spacebar to retry, Esc to quit"
      ]
    ]
  Nothing ->
    [ vBox
      [ renderView appState
      , hBorderWithLabel $ renderProgress appState
      ]
    ]


--
-- Widgets
--

renderProgress appState = txt $ spacepad (elapsedT <> "/" <> totalT)
  where
    elapsedT = timerFormat . floor . fst $ time
    totalT = timerFormat . snd $ time
    time = fromMaybe (0,0) (appState ^. playingStatus . stTimeL)


renderView appState = case appState ^. appView of
  PlaylistView -> renderPlaylist appState
  AddView -> renderBrowser "Add" appState
  OpenView -> renderBrowser "Open" appState


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
      . fromMaybe "-Tag Missing-"
      $ MPD.toText <$> (head =<< Map.lookup tag tags)
      where
        width = (+spacing) . min maxWidth
          $ fromMaybe maxWidth (Map.lookup tag tagsMaxWidths)

    spacing = 3
    tagsMaxWidths = appState ^. playlistTagsMaxWidths

renderBrowser title appState = vBox
  [ hBorderWithLabel (txt $ " " <> title <> " " )
  , L.renderList renderItem True (appState ^. currentDirContents)
  ]
  where
    renderItem selected (Just (MPD.LsDirectory p)) = txt (MPD.toText p)
    renderItem selected (Just (MPD.LsSong p)) = txt (MPD.toText $ MPD.sgFilePath p)
    -- TODO: playlist
    renderItem selected Nothing = txt "All Music"
