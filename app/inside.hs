{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Brick
import           Brick.BChan            (newBChan, writeBChan)
import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Monad          (forever, void)
import           Data.Word
import qualified Graphics.Vty           as V
import qualified Hardware.Vacom.Coldion as CI
import           Lens.Micro
import           Lens.Micro.TH


{- ########################################################################## -}
{- define logic of the device terminal user interface  -}
{- ########################################################################## -}
-- | Measurements on all devices shall be done periodically, therefore we
-- | define a custom data type, named "Tick", which will be fed in as an
-- | event constantly over and over again.
data Tick = Tick

-- | Brick likes to name things. While we do not need names for now, we define
-- | a type, which is empty, so that it is easy to change things later.
type Name = ()

-- | The user interface mainly takes care of displaying things. Therefore
-- | the state of our app is only described by tasks for specific devices, that
-- | shall be carried out in the current loop
data Measurements = Measurements
  { _coldionTask :: CI.CICommand -- what is the Coldion CU-100 supposed to do?
  } deriving (Show)
makeLenses ''Measurements


{- ########################################################################## -}
{- Define the user interface -}
{- ########################################################################## -}
-- | defining the Brick app type
app = App
  { appDraw         = coldionWidget           -- these are the visual elements
                                              -- shown (aka layout), as well as
                                              -- the functionality the have

  , appChooseCursor = neverShowCursor         -- how to handle the cursor and
                                              -- where it is placed now

  , appHandleEvent  = handleEvent             -- a function, describing what
                                              -- happens if a specific event
                                              -- occurs (key press, time passes)

  , appStartEvent   = return                  -- directly start, dont modify
                                              -- anything before we even start

  , appAttrMap      = const theMeasurements   -- a lookup list of styles
                                              -- "const" means that it is never
                                              -- changed
  }

-- | widget for holding the Coldion pressure
coldionWidget :: Measurements -> [Widget Name]
coldionWidget m =
  [ withAttr "pressureAttr" $ str $ show (_coldionTask m) ]

-- | handling tick event
handleEvent :: Measurements -> BrickEvent Name Tick -> EventM Name (Next Measurements)
handleEvent m (AppEvent Tick)                = continue m
handleEvent m (VtyEvent (V.EvKey V.KEsc [])) = halt m

theMeasurements :: AttrMap
theMeasurements = attrMap V.defAttr [("pressureAttr" :: AttrName, V.red `on` V.blue)]

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000

  let initMeasurement = Measurements {_coldionTask = CI.AskPressure 0x01}

  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app initMeasurement
