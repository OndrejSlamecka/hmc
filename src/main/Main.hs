module Main where

import Protolude hiding (State)
import Hmc.Rendering
import Hmc.Types
import Hmc.EventHandler
import Control.Concurrent (ThreadId, threadDelay, forkIO)
import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Util as U
import qualified Brick.BChan as C
import qualified Brick.Widgets.List as L (listSelectedAttr)


main :: IO ()
main = do
  chan <- C.newBChan 10
  tickerThreadId <- forkIO . forever $ do
    C.writeBChan chan Timer
    threadDelay tickerInterval

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
