{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Brick
import           Brick.BChan                              (newBChan, writeBChan)
import qualified Brick.Widgets.Border                     as BB
import qualified Brick.Widgets.Border.Style               as BBS
import qualified Brick.Widgets.Center                     as BC
import           Control.Applicative
import           Control.Concurrent                       (forkIO, threadDelay)
import           Control.Exception                        (catch)
import           Control.Monad                            (forever, void)
import           Control.Monad.IO.Class
import qualified Data.Attoparsec.ByteString.Lazy          as AB
import           Data.Attoparsec.Text.Lazy hiding (take)
import qualified Data.ByteString                          as B
import           Data.Either.Unwrap
import           Data.Maybe
import qualified Data.Text                                as T
import qualified Data.Text.IO                             as T
import           Data.Time
import           Data.Word
import qualified Graphics.Vty                             as V
import qualified Hardware.LakeShore.TemperatureController as LS
import qualified Hardware.Leybold.GraphixThree            as GT
import qualified Hardware.Vacom.Coldion                   as CI
import qualified Internal.Plotting                        as P
import           Lens.Micro
import           Lens.Micro.TH
import           System.Directory
import           System.Environment
import           System.Hardware.Serialport
import           System.IO
import           Text.Printf
import Text.Read


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

-- | The ColdIon interface. What it should do and on which port (something like
-- | /dev/ttyUSB0) the device is connected
type CIPressure = Maybe Double
data ColdIon = ColdIon
  { _ciEnabled    :: Bool
  , _ciTask       :: CI.CICommand
  , _ciPort       :: FilePath
  , _ciChannel    :: Word8
  , _ciPressure   :: CIPressure
  , _ciWarnThresh :: Double
  , _ciDevName    :: Maybe String
  , _ciLabel      :: String
  } deriving (Show)
makeLenses ''ColdIon

-- | Ther LakeShore 335 interface. What it should do and on which port (something
-- | like /dev/ttyUSB0) the device is connected
type LSTemperatures = (Maybe Double, Maybe Double)
data LakeShore = LakeShore
  { _lsEnabled      :: Bool
  , _lsTask         :: LS.LSCommand
  , _lsPort         :: FilePath
  , _lsTemperatures :: LSTemperatures
  , _lsWarnThresh   :: (Double, Double)
  , _lsDevName      :: Maybe String
  , _lsLabel        :: (String, String)
  } deriving (Show)
makeLenses ''LakeShore

-- | The firtst of the to GraphixThree1 controllers, 3 gauges
type GT1Pressures = (Maybe Double, Maybe Double, Maybe Double)
data GraphixThree1 = GraphixThree1
  { _gt1Enabled    :: Bool
  , _gt1Task       :: GT.GTCommand
  , _gt1Port       :: FilePath
  , _gt1Pressures  :: GT1Pressures
  , _gt1WarnThresh :: (Double, Double, Double)
  , _gt1DevName    :: Maybe String
  , _gt1Label      :: (String, String, String)
  } deriving (Show)
makeLenses '' GraphixThree1

-- | The second of the to GraphixThree1 controllers, 3 gauges
type GT2Pressures = (Maybe Double, Maybe Double, Maybe Double)
data GraphixThree2 = GraphixThree2
  { _gt2Enabled    :: Bool
  , _gt2Task       :: GT.GTCommand
  , _gt2Port       :: FilePath
  , _gt2Pressures  :: GT2Pressures
  , _gt2WarnThresh :: (Double, Double, Double)
  , _gt2DevName    :: Maybe String
  , _gt2Label      :: (String, String, String)
  } deriving (Show)
makeLenses '' GraphixThree2

-- | The user interface mainly takes care of displaying things. Therefore
-- | the state of our app is only described by tasks for specific devices, that
-- | shall be carried out in the current loop
data Measurements = Measurements
  { _coldIon       :: ColdIon       -- infos about the Coldion CU-100
  , _lakeShore     :: LakeShore     -- infos about the LakeShore 335
  , _graphixThree1 :: GraphixThree1 -- infos about the first GraphixThree
  , _graphixThree2 :: GraphixThree2 -- infos about the second GraphixThree
  , _writeLog      :: Bool          -- writing everything to a file?
  , _onScreenInfo  :: [String]      -- infos that are raised
  , _plotInterval  :: Int           -- history in Minutes to plot
  } deriving (Show)
makeLenses ''Measurements

data Warning =
    OK
  | OverThresh
  | Invalid
  deriving (Show, Eq)


{- ########################################################################## -}
{- Define the user interface -}
{- ########################################################################## -}
-- | defining the Brick app type
app :: App Measurements Tick Name
app = App
  { appDraw         = drawUI                  -- these are the visual elements
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

-- | horizontal limit (therefore size) of the widgets
hWidgetBoxSize :: Int
hWidgetBoxSize = 30

-- | delay in seconds before new tick is fed into the TUI
tickDistance :: Int
tickDistance = 2500000

-- | the user interface, composed of individual Widgets
drawUI :: Measurements -> [Widget Name]
drawUI m =
  [ BC.center $
    (
      withBorderStyle BBS.unicodeBold
      $ BB.borderWithLabel (str "ColdIon CU-100")
      $ vBox [coldionWidgetPressure m, hLimit hWidgetBoxSize BB.hBorder, deviceWidgetColdIon m, hLimit hWidgetBoxSize BB.hBorder, coldIonWarningWidget m]
    )
  <+>
    (
      withBorderStyle BBS.unicodeBold
      $ BB.borderWithLabel (str "LakeShore 335")
      $ vBox [lakeShoreWidgetTemperature m, hLimit hWidgetBoxSize BB.hBorder, deviceWidgetLakeShore m, hLimit hWidgetBoxSize BB.hBorder, lakeShoreWarningWidget m]
    )
  <+>
    (
      withBorderStyle BBS.unicodeBold
      $ BB.borderWithLabel (str "GraphixThree (1)")
      $ vBox [graphixThree1WidgetPressure m, hLimit hWidgetBoxSize BB.hBorder, deviceWidgetGraphixThree1 m, hLimit hWidgetBoxSize BB.hBorder, graphixThree1WarningWidget m]
    )
  <+>
    (
      withBorderStyle BBS.unicodeBold
      $ BB.borderWithLabel (str "GraphixThree (2)")
      $ vBox [graphixThree2WidgetPressure m, hLimit hWidgetBoxSize BB.hBorder, deviceWidgetGraphixThree2 m, hLimit hWidgetBoxSize BB.hBorder, graphixThree2WarningWidget m]
    )
  <=>
    (
      hLimit ((hWidgetBoxSize + 2) * 4)
      $ withBorderStyle BBS.unicodeBold
      $ BB.borderWithLabel (str "On Screen Infos")
      $ padRight Max
      $ padLeftRight 2
      $ padTopBottom 2
      $ vBox [ str i | i <- m ^. onScreenInfo]
    )
  <=>
    (
      hLimit ((hWidgetBoxSize + 2) * 4)
      $ withBorderStyle BBS.unicodeBold
      $ BB.borderWithLabel (str "Key Bindings")
      $ BC.hCenter
      $ padTopBottom 2
      $ str
        $  "c                    : toggle ColdIon\n"
        ++ "l                    : toggle LakeShore 355\n"
        ++ "g                    : toggle GraphixThree (1)\n"
        ++ "t                    : toggle GraphixThree (2)\n"
        ++ "o                    : toggle Logging\n"
        ++ "+                    : increase plot interval\n"
        ++ "-                    : decrease plot interval\n"
        ++ "Ctrl (c / l / g / t) : plot data for device\n"
        ++ "Space                : enter plot interval manually"

    )
  ]

{- ======= -}
{- ColdIon -}
{- ======= -}
-- | widget for holding the Coldion pressure
coldionWidgetPressure :: Measurements -> Widget Name
coldionWidgetPressure m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ str $ printf "%8s : %8.2e mbar\n\n\n" shownLabel shownPressure
  where
    shownLabel = m ^. coldIon . ciLabel
    shownPressure = fromMaybe 0.0 $ m ^. (coldIon . ciPressure)

-- | showing the parameters of the devices (ports, ...)
deviceWidgetColdIon :: Measurements -> Widget Name
deviceWidgetColdIon m =
  hLimit hWidgetBoxSize
  $ padTopBottom 1
  $ str
  $  ""
  ++ "  Enabled:\n"
  ++ "    " ++ shownEnabled ++ "\n\n"
  ++ "  Port:\n"
  ++ "    " ++ shownPort ++ "\n\n"
  ++ "  Channel:\n"
  ++ "    " ++ shownChannel ++ "\n\n"
  ++ "  Device Name:\n"
  ++ "    " ++ show shownDevName ++ "\n\n"
  ++ "  Warning Threshold:\n"
  ++ "    " ++ show shownWarningThreshold

  where
    shownEnabled = show $ m ^. (coldIon . ciEnabled)
    shownPort = m ^. (coldIon . ciPort)
    shownChannel = show $ m ^. (coldIon . ciChannel)
    shownDevName = m ^. (coldIon . ciDevName)
    shownWarningThreshold = m ^. (coldIon . ciWarnThresh)

coldIonWarningWidget :: Measurements -> Widget Name
coldIonWarningWidget m =
  hLimit hWidgetBoxSize
  $ withAttr attr
  $ BC.hCenter
  $ padTopBottom 1
  $ case coldIonWarning of
      OK         -> str "OK"
      OverThresh -> str "WARNING"
      Invalid    -> str "COMMUNICATION"
  where
    coldIonWarnThresh = m ^. coldIon . ciWarnThresh
    coldIonMonitorVal = m ^. coldIon . ciPressure
    coldIonWarning
      | isNothing coldIonMonitorVal = Invalid
      | fromJust coldIonMonitorVal > coldIonWarnThresh = OverThresh
      | fromJust coldIonMonitorVal <= coldIonWarnThresh = OK
      | otherwise = Invalid
    attr
      | coldIonWarning == OK = "okAttr" :: AttrName
      | coldIonWarning == OverThresh = "warningAttr" :: AttrName
      | coldIonWarning == Invalid = "invalidAttr" :: AttrName
      | otherwise = "invalidAttr" :: AttrName

{- ========= -}
{- LakeShore -}
{- ========= -}
lakeShoreWidgetTemperature :: Measurements -> Widget Name
lakeShoreWidgetTemperature m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ vBox
  [ str $ printf "%8s : %8.2f K\n" shownLabel1 (fromMaybe 0.0 tempA)
  , str $ printf "%8s : %8.2f K\n" shownLabel2 (fromMaybe 0.0 tempB)
  , str "\n"
  ]
  where
    maybeTemps = m ^. lakeShore . lsTemperatures
    tempA = fst maybeTemps
    tempB = snd maybeTemps
    shownLabel1 = m ^. lakeShore . lsLabel . _1
    shownLabel2 = m ^. lakeShore . lsLabel . _2

deviceWidgetLakeShore :: Measurements -> Widget Name
deviceWidgetLakeShore m =
  hLimit hWidgetBoxSize
  $ padTopBottom 1
  $ str
  $  ""
  ++ "  Enabled:\n"
  ++ "    " ++ shownEnabled ++ "\n\n"
  ++ "  Port:\n"
  ++ "    " ++ shownPort ++ "\n\n"
  ++ "\n\n\n"
  ++ "  Device Name:\n"
  ++ "    " ++ show shownDevName ++ "\n\n"
  ++ "  Warning Threshold:\n"
  ++ "    " ++ show shownWarningThreshold
  where
    shownEnabled = show $ m ^. (lakeShore . lsEnabled)
    shownPort = m ^. (lakeShore . lsPort)
    shownDevName = m ^. (lakeShore . lsDevName)
    shownWarningThreshold = m ^. (lakeShore . lsWarnThresh)

lakeShoreWarningWidget :: Measurements -> Widget Name
lakeShoreWarningWidget m =
  hLimit hWidgetBoxSize
  $ withAttr attr
  $ BC.hCenter
  $ padTopBottom 1
  $ case lakeShoreWarning of
      OK         -> str "OK"
      Invalid    -> str "COMMUNICATION"
      OverThresh -> str "WARNING"
  where
    lakeShoreWarnThreshA = m ^. lakeShore . lsWarnThresh . _1
    lakeShoreWarnThreshB = m ^. lakeShore . lsWarnThresh . _2
    lakeShoreMonitorVals =
      ( m ^. lakeShore . lsTemperatures . _1
      , m ^. lakeShore . lsTemperatures . _2
      )
    lakeShoreWarning
      | isNothing (fst lakeShoreMonitorVals) ||
        isNothing (snd lakeShoreMonitorVals)    = Invalid
      | fromJust (fst lakeShoreMonitorVals) > lakeShoreWarnThreshA ||
        fromJust (snd lakeShoreMonitorVals) > lakeShoreWarnThreshB    = OverThresh
      | fromJust (fst lakeShoreMonitorVals) <= lakeShoreWarnThreshA &&
        fromJust (snd lakeShoreMonitorVals) <= lakeShoreWarnThreshB    = OK
      | otherwise = Invalid
    attr
      | lakeShoreWarning == OK = "okAttr" :: AttrName
      | lakeShoreWarning == Invalid = "invalidAttr" :: AttrName
      | lakeShoreWarning == OverThresh = "warningAttr" :: AttrName
      | otherwise = "invalidAttr" :: AttrName


{- ============== -}
{- GraphixThree 1 -}
{- ============== -}
graphixThree1WidgetPressure :: Measurements -> Widget Name
graphixThree1WidgetPressure m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ vBox
  [ str $ printf "%8s : %8.2e mbar\n" shownLabel1 (fromMaybe 0.0 pressureA)
  , str $ printf "%8s : %8.2e mbar\n" shownLabel2 (fromMaybe 0.0 pressureB)
  , str $ printf "%8s : %8.2e mbar\n" shownLabel3 (fromMaybe 0.0 pressureC)
  ]
  where
    maybePressures = m ^. (graphixThree1 . gt1Pressures)
    pressureA = maybePressures ^. _1
    pressureB = maybePressures ^. _2
    pressureC = maybePressures ^. _3
    shownLabel1 = m ^. graphixThree1 . gt1Label . _1
    shownLabel2 = m ^. graphixThree1 . gt1Label . _2
    shownLabel3 = m ^. graphixThree1 . gt1Label . _3

deviceWidgetGraphixThree1 :: Measurements -> Widget Name
deviceWidgetGraphixThree1 m =
  hLimit hWidgetBoxSize
  $ padTopBottom 1
  $ str
  $  ""
  ++ "  Enabled:\n"
  ++ "    " ++ shownEnabled ++ "\n\n"
  ++ "  Port:\n"
  ++ "    " ++ shownPort ++ "\n\n"
  ++ "\n\n\n"
  ++ "  Device Name:\n"
  ++ "    " ++ show shownDevName ++ "\n\n"
  ++ "  Warning Threshold:\n"
  ++ "    " ++ show shownWarningThreshold
  where
    shownEnabled = show $ m ^. (graphixThree1 . gt1Enabled)
    shownPort = m ^. (graphixThree1 . gt1Port)
    shownDevName = m ^. (graphixThree1 . gt1DevName)
    shownWarningThreshold = m ^. (graphixThree1 . gt1WarnThresh)

graphixThree1WarningWidget :: Measurements -> Widget Name
graphixThree1WarningWidget m =
  hLimit hWidgetBoxSize
  $ withAttr attr
  $ BC.hCenter
  $ padTopBottom 1
  $ case graphixThree1Warning of
      OK         -> str "OK"
      Invalid    -> str "COMMUNICATION"
      OverThresh -> str "WARNING"
  where
    graphixThree1WarnThreshA = m ^. graphixThree1 . gt1WarnThresh . _1
    graphixThree1WarnThreshB = m ^. graphixThree1 . gt1WarnThresh . _2
    graphixThree1WarnThreshC = m ^. graphixThree1 . gt1WarnThresh . _3
    graphixThreeMonitorVal = m ^. graphixThree1 . gt1Pressures
    graphixThree1Warning
      | isNothing (graphixThreeMonitorVal ^. _1) ||
        isNothing (graphixThreeMonitorVal ^. _2) ||
        isNothing (graphixThreeMonitorVal ^. _3)    = Invalid
      | fromJust (graphixThreeMonitorVal ^. _1) > graphixThree1WarnThreshA ||
        fromJust (graphixThreeMonitorVal ^. _2) > graphixThree1WarnThreshB ||
        fromJust (graphixThreeMonitorVal ^. _3) > graphixThree1WarnThreshC    = OverThresh
      | fromJust (graphixThreeMonitorVal ^. _1) <= graphixThree1WarnThreshA &&
        fromJust (graphixThreeMonitorVal ^. _2) <= graphixThree1WarnThreshB &&
        fromJust (graphixThreeMonitorVal ^. _3) <= graphixThree1WarnThreshC    = OK
      | otherwise = Invalid
    attr
      | graphixThree1Warning == OK = "okAttr"   :: AttrName
      | graphixThree1Warning == Invalid = "invalidAttr" :: AttrName
      | graphixThree1Warning == OverThresh = "warningAttr" :: AttrName
      | otherwise = "invalidAttr" :: AttrName

{- ============== -}
{- GraphixThree 2 -}
{- ============== -}
graphixThree2WidgetPressure :: Measurements -> Widget Name
graphixThree2WidgetPressure m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ vBox
  [ str $ printf "%8s : %8.2e mbar\n" shownLabel1 (fromMaybe 0.0 pressureA)
  , str $ printf "%8s : %8.2e mbar\n" shownLabel2 (fromMaybe 0.0 pressureB)
  , str $ printf "%8s : %8.2e mbar\n" shownLabel3 (fromMaybe 0.0 pressureC)
  ]
  where
    maybePressures = m ^. (graphixThree2 . gt2Pressures)
    pressureA = maybePressures ^. _1
    pressureB = maybePressures ^. _2
    pressureC = maybePressures ^. _3
    shownLabel1 = m ^. graphixThree2 . gt2Label . _1
    shownLabel2 = m ^. graphixThree2 . gt2Label . _2
    shownLabel3 = m ^. graphixThree2 . gt2Label . _3

deviceWidgetGraphixThree2 :: Measurements -> Widget Name
deviceWidgetGraphixThree2 m =
  hLimit hWidgetBoxSize
  $ padTopBottom 1
  $ str
  $  ""
  ++ "  Enabled:\n"
  ++ "    " ++ shownEnabled ++ "\n\n"
  ++ "  Port:\n"
  ++ "    " ++ shownPort ++ "\n\n"
  ++ "\n\n\n"
  ++ "  Device Name:\n"
  ++ "    " ++ show shownDevName ++ "\n\n"
  ++ "  Warning Threshold:\n"
  ++ "    " ++ show shownWarningThreshold
  where
    shownEnabled = show $ m ^. (graphixThree2 . gt2Enabled)
    shownPort = m ^. (graphixThree2 . gt2Port)
    shownDevName = m ^. (graphixThree2 . gt2DevName)
    shownWarningThreshold = m ^. (graphixThree2 . gt2WarnThresh)

graphixThree2WarningWidget :: Measurements -> Widget Name
graphixThree2WarningWidget m =
  hLimit hWidgetBoxSize
  $ withAttr attr
  $ BC.hCenter
  $ padTopBottom 1
  $ case graphixThree2Warning of
      OK         -> str "OK"
      Invalid    -> str "COMMUNICATION"
      OverThresh -> str "WARNING"
  where
    graphixThree2WarnThreshA = m ^. graphixThree2 . gt2WarnThresh . _1
    graphixThree2WarnThreshB = m ^. graphixThree2 . gt2WarnThresh . _2
    graphixThree2WarnThreshC = m ^. graphixThree2 . gt2WarnThresh . _3
    graphixThreeMonitorVal = m ^. graphixThree2 . gt2Pressures
    graphixThree2Warning
      | isNothing (graphixThreeMonitorVal ^. _1) ||
        isNothing (graphixThreeMonitorVal ^. _2) ||
        isNothing (graphixThreeMonitorVal ^. _3)    = Invalid
      | fromJust (graphixThreeMonitorVal ^. _1) > graphixThree2WarnThreshA ||
        fromJust (graphixThreeMonitorVal ^. _2) > graphixThree2WarnThreshB ||
        fromJust (graphixThreeMonitorVal ^. _3) > graphixThree2WarnThreshC    = OverThresh
      | fromJust (graphixThreeMonitorVal ^. _1) <= graphixThree2WarnThreshA &&
        fromJust (graphixThreeMonitorVal ^. _2) <= graphixThree2WarnThreshB &&
        fromJust (graphixThreeMonitorVal ^. _3) <= graphixThree2WarnThreshC    = OK
      | otherwise = Invalid
    attr
      | graphixThree2Warning == OK = "okAttr"   :: AttrName
      | graphixThree2Warning == Invalid = "invalidAttr" :: AttrName
      | graphixThree2Warning == OverThresh = "warningAttr" :: AttrName
      | otherwise = "invalidAttr" :: AttrName


{- ########################################################################## -}
{- Define EventHandling -}
{- ########################################################################## -}
-- | handling events that occur. These are the ticks, fed into the app as well
-- | as the keys pressed
handleEvent :: Measurements -> BrickEvent Name Tick -> EventM Name (Next Measurements)
handleEvent m (AppEvent Tick) =
   liftIO (getCurrentConditions m) >>= continue

handleEvent m (VtyEvent (V.EvKey (V.KChar 'c') [])) =
  continue $ toggleColdIon m

handleEvent m (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) =
  liftIO
    ( catch (plotter [P.ColdIon] m) (plotterHandler m)
    ) >>= continue

handleEvent m (VtyEvent (V.EvKey (V.KChar 'l') [])) =
  continue $ toggleLakeShore m

handleEvent m (VtyEvent (V.EvKey (V.KChar 'l') [V.MCtrl])) =
  liftIO
    ( catch (plotter [P.LakeShore LS.A, P.LakeShore LS.B] m) (plotterHandler m)
    ) >>= continue

handleEvent m (VtyEvent (V.EvKey (V.KChar 'g') [])) =
  continue $ toggleGraphixThree1 m

handleEvent m (VtyEvent (V.EvKey (V.KChar 'g') [V.MCtrl])) =
  liftIO
    ( catch (plotter [P.GraphixThree1 GT.A, P.GraphixThree1 GT.B, P.GraphixThree1 GT.C] m) (plotterHandler m)
    ) >>= continue

handleEvent m (VtyEvent (V.EvKey (V.KChar 't') [])) =
  continue $ toggleGraphixThree2 m

handleEvent m (VtyEvent (V.EvKey (V.KChar 't') [V.MCtrl])) =
  liftIO
    ( catch (plotter [P.GraphixThree2 GT.A, P.GraphixThree2 GT.B, P.GraphixThree2 GT.C] m) (plotterHandler m)
    ) >>= continue

handleEvent m (VtyEvent (V.EvKey (V.KChar 'o') [])) =
  continue $ toggleLogging m

handleEvent m (VtyEvent (V.EvKey (V.KChar '+') [])) =
  continue $ incrPlotInterval m

handleEvent m (VtyEvent (V.EvKey (V.KChar '-') [])) =
  continue $ decrPlotInterval m

handleEvent m (VtyEvent (V.EvKey (V.KChar ' ') [])) =
  suspendAndResume $ do
    putStrLn "Suspended! Enter new plot interval in minutes."
    pI <- getLine
    let maybeMinutes = (readMaybe :: String -> Maybe Int) pI
    if isJust maybeMinutes
      then return $ m & plotInterval .~ fromJust maybeMinutes
      else return m

handleEvent m (VtyEvent (V.EvKey V.KEsc [])) =
   halt m

handleEvent m _ =
   continue m

plotterHandler :: Measurements -> IOError -> IO Measurements
plotterHandler m e = return $
    m & onScreenInfo .~ oldOnScreenInfo ++ ["Plotting : FAILED with " ++ show e]
 where
   oldOnScreenInfo = m ^. onScreenInfo

-- | Request an update of all currently connected devices.
-- | This is basically a wrapper function around all the individual update
-- | function for the devices
getCurrentConditions :: Measurements -> IO Measurements
getCurrentConditions m = do
  let mOnScreenInfoReset = m & onScreenInfo .~ ["Waiting ..."]
  newColdIonMeasurements <-
    if (m ^. coldIon . ciEnabled)
      then catch (updateColdIonPressure mOnScreenInfoReset) (ciHandler mOnScreenInfoReset)
      else return mOnScreenInfoReset

  newLakeShoreMeasurements <-
    if (m ^. lakeShore . lsEnabled)
      then catch (updateLakeShoreTemperatures mOnScreenInfoReset) (lsHandler mOnScreenInfoReset)
      else return mOnScreenInfoReset

  newGraphixThree1Measurements <-
    if (m ^. graphixThree1 . gt1Enabled)
      then catch (updateGraphixThree1Pressures mOnScreenInfoReset) (gt1Handler mOnScreenInfoReset)
      else return mOnScreenInfoReset

  newGraphixThree2Measurements <-
    if (mOnScreenInfoReset ^. graphixThree2 . gt2Enabled)
      then catch (updateGraphixThree2Pressures mOnScreenInfoReset) (gt2Handler mOnScreenInfoReset)
      else return mOnScreenInfoReset

  if (mOnScreenInfoReset ^. writeLog)
    then logWriter mOnScreenInfoReset
    else return ()

  return $
    m
    & coldIon .~ (newColdIonMeasurements ^. coldIon)
    & lakeShore .~ (newLakeShoreMeasurements ^. lakeShore)
    & graphixThree1 .~ (newGraphixThree1Measurements ^. graphixThree1)
    & graphixThree2 .~ (newGraphixThree2Measurements ^. graphixThree2)
    & onScreenInfo .~
      [head (newColdIonMeasurements ^. onScreenInfo)]
      ++
      [head (newLakeShoreMeasurements ^. onScreenInfo)]
      ++
      [head (newGraphixThree1Measurements ^. onScreenInfo)]
      ++
      [head (newGraphixThree2Measurements ^. onScreenInfo)]
      ++
      ["Logging       : " ++ show (mOnScreenInfoReset ^. writeLog)]
      ++
      ["Plot Interval : " ++ show (mOnScreenInfoReset ^. plotInterval) ++ " min"]

  where
    updateColdIonPressure :: Measurements -> IO Measurements
    updateColdIonPressure n = do
      updatedColdIonPressure <- coldIonPressureUpdate n
      return $
        n
        & coldIon . ciPressure .~ updatedColdIonPressure
        & onScreenInfo .~ ["ColdIon CU-100 : connection OK"]
    ciHandler :: Measurements -> IOError -> IO Measurements
    ciHandler n e =
      return $
        n
        & onScreenInfo .~ ["ColdIon CU-100 : connection FAILED with " ++ show e]
        & coldIon . ciPressure .~ Nothing

    updateLakeShoreTemperatures :: Measurements -> IO Measurements
    updateLakeShoreTemperatures n = do
      updatedLakeShoreTemperatures <- lakeShoreTempUpdate n
      return $
        n
        & lakeShore . lsTemperatures .~ updatedLakeShoreTemperatures
        & onScreenInfo .~ ["LakeShore 355 : connection OK"]
    lsHandler :: Measurements -> IOError -> IO Measurements
    lsHandler n e =
      return $
        n
        & onScreenInfo .~ ["LakeShore 355 : connection FAILED with " ++ show e]
        & lakeShore . lsTemperatures .~ (Nothing, Nothing)

    updateGraphixThree1Pressures :: Measurements -> IO Measurements
    updateGraphixThree1Pressures n = do
      updatedGraphixThree1Pressures <- graphixThree1PressureUpdate n
      return $
        n
        & graphixThree1 . gt1Pressures .~ updatedGraphixThree1Pressures
        & onScreenInfo .~ ["graphixThree (1) : connection OK"]
    gt1Handler :: Measurements -> IOError -> IO Measurements
    gt1Handler n e =
      return $
        n
        & onScreenInfo .~ ["graphixThree (1) : connection FAILED with " ++ show e]
        & graphixThree1 . gt1Pressures .~ (Nothing, Nothing, Nothing)

    updateGraphixThree2Pressures :: Measurements -> IO Measurements
    updateGraphixThree2Pressures n = do
      updatedGraphixThree2Pressures <- graphixThree2PressureUpdate n
      return $
        n
        & graphixThree2 . gt2Pressures .~ updatedGraphixThree2Pressures
        & onScreenInfo .~ ["graphixThree (2) : connection OK"]
    gt2Handler :: Measurements -> IOError -> IO Measurements
    gt2Handler n e =
      return $
        n
        & onScreenInfo .~ ["graphixThree (2) : connection FAILED with " ++ show e]
        & graphixThree2 . gt2Pressures .~ (Nothing, Nothing, Nothing)

{- ======= -}
{- Logging -}
{- ======= -}
-- | Writes a log file with all conditions of the current cycle. Log rotation at
-- | midnight
logWriter :: Measurements -> IO ()
logWriter m = do
  -- ask the system for the current day
  logDate <- do
    today <- getZonedTime
    return $ localDay . zonedTimeToLocalTime $ today
  -- the log name contains the current date. If going beyond midnight, a new
  -- name will be created and a new log file will be started
  let logName = "inside_" ++ show logDate ++ ".log"

  logAppend <- doesFileExist logName

  if logAppend
    then do
      logFile <- openFile logName AppendMode
      writeLogLine logFile m
      hClose logFile
    else do
      logFile <- openFile logName WriteMode
      writeHeader logFile m
      writeLogLine logFile m
      hClose logFile

  where
    writeHeader :: Handle -> Measurements -> IO ()
    writeHeader handle n = do
      --              time      ci       lsA    lsB      gt1A   gt1B   gt1C     gt2A   gt2B   gt2C
      hPrintf handle "#%-33s    #%-8s    #%-8s  #%-8s    #%-8s  #%-8s  #%-8s    #%-8s  #%-8s  #%-8s\n"
        ("Time" :: String) ci lsA lsB gt1A gt1B gt1C gt2A gt2B gt2C
        where
          ci = n ^. coldIon . ciLabel
          lsA = n ^. lakeShore . lsLabel . _1
          lsB = n ^. lakeShore . lsLabel . _2
          gt1A = n ^. graphixThree1 . gt1Label . _1
          gt1B = n ^. graphixThree1 . gt1Label . _2
          gt1C = n ^. graphixThree1 . gt1Label . _3
          gt2A = n ^. graphixThree2 . gt2Label . _1
          gt2B = n ^. graphixThree2 . gt2Label . _2
          gt2C = n ^. graphixThree2 . gt2Label . _3

    writeLogLine :: Handle -> Measurements -> IO ()
    writeLogLine handle n = do
      logTime <- getZonedTime

      hPrintf handle "%-33s    %8.2e    %8.2f  %8.2f    %8.2e  %8.2e  %8.2e    %8.2e  %8.2e  %8.2e\n"
        (show logTime) ci lsA lsB gt1A gt1B gt1C gt2A gt2B gt2C

        where
          ci = fromMaybe 0.0 $ n ^. (coldIon . ciPressure)
          lsA = fromMaybe 0.0 $ n ^. lakeShore . lsTemperatures . _1
          lsB = fromMaybe 0.0 $ n ^. lakeShore . lsTemperatures . _2
          gt1A = fromMaybe 0.0 $ n ^. graphixThree1 . gt1Pressures . _1
          gt1B = fromMaybe 0.0 $ n ^. graphixThree1 . gt1Pressures . _2
          gt1C = fromMaybe 0.0 $ n ^. graphixThree1 . gt1Pressures . _3
          gt2A = fromMaybe 0.0 $ n ^. graphixThree2 . gt2Pressures . _1
          gt2B = fromMaybe 0.0 $ n ^. graphixThree2 . gt2Pressures . _2
          gt2C = fromMaybe 0.0 $ n ^. graphixThree2 . gt2Pressures . _3

toggleLogging :: Measurements -> Measurements
toggleLogging m = m & writeLog .~ not currVal
  where
    currVal = m ^. writeLog

{- ======== -}
{- Plotting -}
{- ======== -}
-- | plot for a selected device the conditions in the last minutes to a file
plotter :: [P.PlotDevice] -> Measurements -> IO Measurements
plotter device m = do
  -- get the current date to construct the name of the current log file, so that
  -- only the most recent will be read
  logDate <- do
    today <- getZonedTime
    return $ localDay . zonedTimeToLocalTime $ today
  let logName = "inside_" ++ show logDate ++ ".log"

  -- read and parse the log file
  logCont <- T.readFile logName
  let latestLogP = parseOnly P.parsePlot logCont

  --
  if isRight latestLogP
    then do
      let latestLog = fromRight latestLogP
          takeable = length latestLog > linesToTake

      if takeable
        then do
          let logInInterval = take linesToTake latestLog
          mapM_ (\d -> P.plotSelectedLogData d logInInterval (devFileName d)) device
          return $
            m & onScreenInfo .~ oldOnScreenInfo ++ ["\nPlotting : OK"]
        else return $
               m & onScreenInfo .~ oldOnScreenInfo ++ ["\nPlotting : FAILED, not enough lines, need " ++ show linesToTake ++ "lines"]
    else return $
           m & onScreenInfo .~ oldOnScreenInfo ++ ["\nPlotting : FAILED, cannot read log file"]
  where
    intervalInMins = m ^. plotInterval
    linesPerMinute =
      (round :: Double -> Int) $ 60.0 / (fromIntegral tickDistance / 1000000.0) :: Int
    linesToTake = intervalInMins * linesPerMinute
    devFileName :: P.PlotDevice -> FilePath
    devFileName d
      | d == P.ColdIon = "ColdIonPlot.svg"
      | d == P.LakeShore LS.A = "LakeShore_A.svg"
      | d == P.LakeShore LS.B = "LakeShore_B.svg"
      | d == P.GraphixThree1 GT.A = "GraphixThree1_A.svg"
      | d == P.GraphixThree1 GT.B = "GraphixThree1_B.svg"
      | d == P.GraphixThree1 GT.C = "GraphixThree1_C.svg"
      | d == P.GraphixThree2 GT.A = "GraphixThree2_A.svg"
      | d == P.GraphixThree2 GT.B = "GraphixThree2_B.svg"
      | d == P.GraphixThree2 GT.C = "GraphixThree2_C.svg"
      | otherwise = "IHaveNoIdeaWhatIAmDoingHere.svg"
    oldOnScreenInfo = m ^. onScreenInfo

incrPlotInterval :: Measurements -> Measurements
incrPlotInterval m = m & plotInterval .~ (oldInterval + 1)
  where
    oldInterval = m ^. plotInterval

decrPlotInterval :: Measurements -> Measurements
decrPlotInterval m
  | oldInterval <= 1 = m
  | otherwise = m & plotInterval .~ (oldInterval - 1)
  where
    oldInterval = m ^. plotInterval

{- ======= -}
{- ColdIon -}
{- ======= -}
-- | Sending, receiving and understanding an request to the ColdIon.
-- | The pressure is retured
coldIonPressureUpdate :: Measurements -> IO CIPressure
coldIonPressureUpdate m = do
  ci <- openSerial coldIonPort defaultSerialSettings { commSpeed = CS19200 }
  _ <- send ci $ CI.ciString2ByteString . fromJust $ coldIonRequest
  threadDelay 50000
  coldIonAnswer <- recv ci 255
  closeSerial ci

  -- This is perfectly safe function returning Nothing if it fails.
  -- nevertheless; it is an IO function, therefore we have IO type
  let coldIonAnswerCIString = AB.parseOnly CI.parseAnswer coldIonAnswer

  if isRight coldIonAnswerCIString
    then do
      let coldIonPressure =
            AB.parseOnly CI.parsePressure
            $ B.pack
            . CI.dataBytes2List
            . CI._ci_data
            $ fromRight coldIonAnswerCIString

      if (isRight coldIonPressure)
        then do
          --print coldIonPressure
          --print $ isRight coldIonPressure
          return $ Just (fromRight coldIonPressure)
        else do
          --print coldIonPressure
          --print $ isRight coldIonPressure
          return $ Nothing
    else do
      --print coldIonAnswerCIString
      return Nothing
  where
    coldIonPort = m ^. coldIon . ciPort
    coldIonChannel = m ^. coldIon . ciChannel
    coldIonRequest = CI.createCommandCIString (CI.AskPressure coldIonChannel)

-- | Sending, receiving and understanding an request to the ColdIon.
-- | The Name is retured
coldIonNameUpdate :: Measurements -> IO (Maybe String)
coldIonNameUpdate m = do
  ci <- openSerial coldIonPort defaultSerialSettings { commSpeed = CS19200 }
  _ <- send ci $ CI.ciString2ByteString . fromJust $ coldIonRequest
  coldIonAnswer <- recv ci 24
  closeSerial ci

  -- This is perfectly safe function returning Nothing if it fails.
  -- nevertheless; it is an IO function, therefore we have IO type
  let coldIonAnswerCIString = AB.parseOnly CI.parseAnswer coldIonAnswer
  if isRight coldIonAnswerCIString
    then do
      let coldIonDevName =
            AB.parseOnly CI.parseName
            $ B.pack
            . CI.dataBytes2List
            . CI._ci_data
            $ fromRight coldIonAnswerCIString

      if isRight coldIonDevName
        then return $ Just (fromRight coldIonDevName)
        else return Nothing
    else return Nothing
  where
    coldIonPort = m ^. coldIon . ciPort
    coldIonRequest = CI.createCommandCIString CI.GetDevName

-- | toggle the Update of the ColdIon (is enabled -> disable and vice versa)
toggleColdIon :: Measurements -> Measurements
toggleColdIon m = m & coldIon . ciEnabled .~ not currVal
  where
    currVal = m ^. coldIon . ciEnabled

{- ========= -}
{- LakeShore -}
{- ========= -}
lakeShoreTempUpdate :: Measurements -> IO (Maybe Double, Maybe Double)
lakeShoreTempUpdate m = do
  ls <- openSerial lakeShorePort defaultSerialSettings {commSpeed = CS57600, bitsPerWord = 7, parity = Odd}
  _ <- send ls $ fromJust lakeShoreRequestA
  lakeShoreAnswerA <- recv ls 255

  -- the LakeShore needs some time before next request can be understood
  threadDelay 200000

  _ <- send ls $ fromJust lakeShoreRequestB
  lakeShoreAnswerB <- recv ls 255
  closeSerial ls

  let lakeShoreTempA = AB.parseOnly LS.parseTemperature lakeShoreAnswerA
      lakeShoreTempB = AB.parseOnly LS.parseTemperature lakeShoreAnswerB

  if isLeft lakeShoreTempA
    then
      if isLeft lakeShoreTempB
        then return (Nothing, Nothing)
        else return (Nothing, Just $ fromRight lakeShoreTempB)
    else
      if isLeft lakeShoreTempB
        then return (Just $ fromRight lakeShoreTempA, Nothing)
        else return (Just $ fromRight lakeShoreTempA, Just $ fromRight lakeShoreTempB)
  where
    lakeShorePort = m ^. lakeShore . lsPort
    lakeShoreRequestA = LS.createCommandLSString (LS.AskTemperature LS.A)
    lakeShoreRequestB = LS.createCommandLSString (LS.AskTemperature LS.B)

toggleLakeShore :: Measurements -> Measurements
toggleLakeShore m = m & lakeShore . lsEnabled .~ not currVal
  where
    currVal = m ^. lakeShore . lsEnabled

{- ============== -}
{- GraphixThree 1 -}
{- ============== -}
-- | update all three readings from the GraphixThree (Number 1)
-- | if a gauge is not connected or transmitting data fails,
-- | a Nothing is returned
graphixThree1PressureUpdate :: Measurements -> IO (Maybe Double, Maybe Double, Maybe Double)
graphixThree1PressureUpdate m = do
  gt1 <- openSerial graphixThree1Port defaultSerialSettings { commSpeed = CS38400 }

  threadDelay 5000

  -- request pressure from gauge 1
  _ <- send gt1 $ fromJust graphixThreeRequestA
  graphixThree1AnswerA <- recv gt1 255

  threadDelay 100000

-- request pressure from gauge 2
  _ <- send gt1 $ fromJust graphixThreeRequestB
  graphixThree1AnswerB <- recv gt1 255

  threadDelay 100000

  -- request pressure from gauge 3
  _ <- send gt1 $ fromJust graphixThreeRequestC
  graphixThree1AnswerC <- recv gt1 255

  closeSerial gt1

  let graphixThree1TempA = AB.parseOnly GT.parsePressure graphixThree1AnswerA
      graphixThree1TempB = AB.parseOnly GT.parsePressure graphixThree1AnswerB
      graphixThree1TempC = AB.parseOnly GT.parsePressure graphixThree1AnswerC

  if isLeft graphixThree1TempA
    then
      if isLeft graphixThree1TempB
        then
          if isLeft graphixThree1TempC
            then return (Nothing, Nothing, Nothing)
            else return (Nothing, Nothing, Just $ fromRight graphixThree1TempA)
        else
          if isLeft graphixThree1TempC
            then return (Nothing, Just $ fromRight graphixThree1TempB, Nothing)
            else return (Nothing, Just $ fromRight graphixThree1TempB, Just $ fromRight graphixThree1TempC)
    else
      if isLeft graphixThree1TempB
        then
          if isLeft graphixThree1TempC
            then return (Just $ fromRight graphixThree1TempA, Nothing, Nothing)
            else return (Just $ fromRight graphixThree1TempA, Nothing, Just $ fromRight graphixThree1TempC)
        else
          if isLeft graphixThree1TempC
            then return (Just $ fromRight graphixThree1TempA, Just $ fromRight graphixThree1TempB, Nothing)
            else return (Just $ fromRight graphixThree1TempA, Just $ fromRight graphixThree1TempB, Just $ fromRight graphixThree1TempC)
  where
    graphixThree1Port = m ^. graphixThree1 . gt1Port
    graphixThreeRequestA = GT.createCommandGTString (GT.AskPressure GT.A)
    graphixThreeRequestB = GT.createCommandGTString (GT.AskPressure GT.B)
    graphixThreeRequestC = GT.createCommandGTString (GT.AskPressure GT.C)

toggleGraphixThree1 :: Measurements -> Measurements
toggleGraphixThree1 m = m & graphixThree1 . gt1Enabled .~ not currVal
  where
    currVal = m ^. graphixThree1 . gt1Enabled

{- ============== -}
{- GraphixThree 2 -}
{- ============== -}
-- | update all three readings from the GraphixThree (Number 1)
-- | if a gauge is not connected or transmitting data fails,
-- | a Nothing is returned
graphixThree2PressureUpdate :: Measurements -> IO (Maybe Double, Maybe Double, Maybe Double)
graphixThree2PressureUpdate m = do
  gt2 <- openSerial graphixThree2Port defaultSerialSettings { commSpeed = CS38400 }
  _ <- send gt2 $ fromJust graphixThreeRequestA
  graphixThree2AnswerA <- recv gt2 255

  threadDelay 100000

  _ <- send gt2 $ fromJust graphixThreeRequestB
  graphixThree2AnswerB <- recv gt2 255

  threadDelay 100000

  _ <- send gt2 $ fromJust graphixThreeRequestC
  graphixThree2AnswerC <- recv gt2 255

  closeSerial gt2

  let graphixThree2TempA = AB.parseOnly GT.parsePressure graphixThree2AnswerA
      graphixThree2TempB = AB.parseOnly GT.parsePressure graphixThree2AnswerB
      graphixThree2TempC = AB.parseOnly GT.parsePressure graphixThree2AnswerC

  if isLeft graphixThree2TempA
    then
      if isLeft graphixThree2TempB
        then
          if isLeft graphixThree2TempC
            then return (Nothing, Nothing, Nothing)
            else return (Nothing, Nothing, Just $ fromRight graphixThree2TempA)
        else
          if isLeft graphixThree2TempC
            then return (Nothing, Just $ fromRight graphixThree2TempB, Nothing)
            else return (Nothing, Just $ fromRight graphixThree2TempB, Just $ fromRight graphixThree2TempC)
    else
      if isLeft graphixThree2TempB
        then
          if isLeft graphixThree2TempC
            then return (Just $ fromRight graphixThree2TempA, Nothing, Nothing)
            else return (Just $ fromRight graphixThree2TempA, Nothing, Just $ fromRight graphixThree2TempC)
        else
          if isLeft graphixThree2TempC
            then return (Just $ fromRight graphixThree2TempA, Just $ fromRight graphixThree2TempB, Nothing)
            else return (Just $ fromRight graphixThree2TempA, Just $ fromRight graphixThree2TempB, Just $ fromRight graphixThree2TempC)
  where
    graphixThree2Port = m ^. graphixThree2 . gt2Port
    graphixThreeRequestA = GT.createCommandGTString (GT.AskPressure GT.A)
    graphixThreeRequestB = GT.createCommandGTString (GT.AskPressure GT.B)
    graphixThreeRequestC = GT.createCommandGTString (GT.AskPressure GT.C)

toggleGraphixThree2 :: Measurements -> Measurements
toggleGraphixThree2 m = m & graphixThree2 . gt2Enabled .~ not currVal
  where
    currVal = m ^. graphixThree2 . gt2Enabled


{- ########################################################################## -}
{- Appearance of the widgets, defining Attributes -}
{- ########################################################################## -}
theMeasurements :: AttrMap
theMeasurements = attrMap V.defAttr
  [ ("normalAttr"  :: AttrName, V.white `on` V.black)
  , ("warnAttr"    :: AttrName, V.blue `on` V.red)
  , ("okAttr"      :: AttrName, V.blue `on` V.green)
  , ("invalidAttr" :: AttrName, V.blue `on` V.yellow)
  ]


{- ########################################################################## -}
{- Parsers -}
{- ########################################################################## -}
-- | parsing the defaults from a file and using defaults, that are not parsed
-- | from the standard definition
defaultsParser :: Parser Measurements
defaultsParser = do
  defColdIon <- coldIonParser
  skipSpace
  defLakeShore <- lakeShoreParser
  skipSpace
  defGraphixThree1 <- graphixThree1Parser
  skipSpace
  defGraphixThree2 <- graphixThree2Parser
  skipSpace
  _ <- string $ T.pack "[Logging]"
  skipSpace
  _ <- string $ T.pack "enabled ="
  skipSpace
  enabledP <- (string $ T.pack "True") <|> (string $ T.pack "False")
  return $
    initMeasurement
    & coldIon .~ defColdIon
    & lakeShore .~ defLakeShore
    & graphixThree1 .~ defGraphixThree1
    & graphixThree2 .~ defGraphixThree2
    & writeLog .~ (enabledP == T.pack "True")

-- | parse the ColdIon block and update the defaults with the parsed result
coldIonParser :: Parser ColdIon
coldIonParser = do
  _ <- string $ T.pack "[ColdIon]"
  skipSpace
  _ <- string $ T.pack "enabled ="
  skipSpace
  enabledP <- (string $ T.pack "True") <|> (string $ T.pack "False")
  skipSpace
  _ <- string $ T.pack "port ="
  skipSpace
  portP <- manyTill anyChar endOfLine
  skipSpace
  _ <- string $ T.pack "channel ="
  skipSpace
  channelP <- decimal
  skipSpace
  _ <- string $ T.pack "warn ="
  skipSpace
  warnP <- double
  skipSpace
  _ <- string $ T.pack "label ="
  skipSpace
  label1P <- manyTill anyChar endOfLine
  return $
    initColdIon
    & ciEnabled .~ (enabledP == T.pack "True")
    & ciPort .~ portP
    & ciChannel .~ channelP
    & ciWarnThresh .~ warnP
    & ciLabel .~ label1P

lakeShoreParser :: Parser LakeShore
lakeShoreParser = do
  _ <- string $ T.pack "[LakeShore355]"
  skipSpace
  _ <- string $ T.pack "enabled ="
  skipSpace
  enabledP <- (string $ T.pack "True") <|> (string $ T.pack "False")
  skipSpace
  _ <- string $ T.pack "port ="
  skipSpace
  portP <- manyTill anyChar endOfLine
  skipSpace
  _ <- string $ T.pack "warn ="
  skipSpace
  warn1P <- double
  skipSpace
  warn2P <- double
  skipSpace
  _ <- string $ T.pack "label ="
  skipSpace
  label1P <- manyTill anyChar (char ' ')
  label2P <- manyTill anyChar endOfLine

  return $
    initLakeShore
    & lsEnabled .~ (enabledP == T.pack "True")
    & lsPort .~ portP
    & lsWarnThresh .~ (warn1P, warn2P)
    & lsLabel .~ (label1P, label2P)

graphixThree1Parser :: Parser GraphixThree1
graphixThree1Parser = do
  _ <- string $ T.pack "[GraphixThree1]"
  skipSpace
  _ <- string $ T.pack "enabled ="
  skipSpace
  enabledP <- (string $ T.pack "True") <|> (string $ T.pack "False")
  skipSpace
  _ <- string $ T.pack "port ="
  skipSpace
  portP <- manyTill anyChar endOfLine
  skipSpace
  _ <- string $ T.pack "warn ="
  skipSpace
  warn1P <- double
  skipSpace
  warn2P <- double
  skipSpace
  warn3P <- double
  skipSpace
  _ <- string $ T.pack "label ="
  skipSpace
  label1P <- manyTill anyChar (char ' ')
  label2P <- manyTill anyChar (char ' ')
  label3P <- manyTill anyChar endOfLine

  return $
    initGraphixThree1
    & gt1Enabled .~ (enabledP == T.pack "True")
    & gt1Port .~ portP
    & gt1WarnThresh .~ (warn1P, warn2P, warn3P)
    & gt1Label .~ (label1P, label2P, label3P)

graphixThree2Parser :: Parser GraphixThree2
graphixThree2Parser = do
  _ <- string $ T.pack "[GraphixThree2]"
  skipSpace
  _ <- string $ T.pack "enabled ="
  skipSpace
  enabledP <- (string $ T.pack "True") <|> (string $ T.pack "False")
  skipSpace
  _ <- string $ T.pack "port ="
  skipSpace
  portP <- manyTill anyChar endOfLine
  skipSpace
  _ <- string $ T.pack "warn ="
  skipSpace
  warn1P <- double
  skipSpace
  warn2P <- double
  skipSpace
  warn3P <- double
  skipSpace
  _ <- string $ T.pack "label ="
  skipSpace
  label1P <- manyTill anyChar (char ' ')
  label2P <- manyTill anyChar (char ' ')
  label3P <- manyTill anyChar endOfLine

  return $
    initGraphixThree2
    & gt2Enabled .~ (enabledP == T.pack "True")
    & gt2Port .~ portP
    & gt2WarnThresh .~ (warn1P, warn2P, warn3P)
    & gt2Label .~ (label1P, label2P, label3P)


{- ########################################################################## -}
{- Defaults for the Measurement State -}
{- ########################################################################## -}
initColdIon :: ColdIon
initColdIon = ColdIon
  { _ciEnabled    = False
  , _ciTask       = CI.GetDevName
  , _ciPort       = "/dev/ttyUSB0"
  , _ciChannel    = 1
  , _ciPressure   = Nothing
  , _ciWarnThresh = 1.0e-9
  , _ciDevName    = Nothing
  , _ciLabel      = "A"
  }

initLakeShore :: LakeShore
initLakeShore = LakeShore
  { _lsEnabled      = False
  , _lsTask         = LS.AskTemperature LS.A
  , _lsPort         = "/dev/ttyUSB1"
  , _lsTemperatures = (Nothing, Nothing)
  , _lsWarnThresh   = (300.0, 300.0)
  , _lsDevName      = Nothing
  , _lsLabel        = ("A", "B")
  }

initGraphixThree1 :: GraphixThree1
initGraphixThree1  = GraphixThree1
  { _gt1Enabled    = False
  , _gt1Task       = GT.AskPressure GT.A
  , _gt1Port       = "/dev/ttyUSB2"
  , _gt1Pressures  = (Nothing, Nothing, Nothing)
  , _gt1WarnThresh = (1050.0, 1050.0, 1050.0)
  , _gt1DevName    = Nothing
  , _gt1Label      = ("A", "B", "C")
  }

initGraphixThree2 :: GraphixThree2
initGraphixThree2  = GraphixThree2
  { _gt2Enabled    = False
  , _gt2Task       = GT.AskPressure GT.A
  , _gt2Port       = "/dev/ttyUSB3"
  , _gt2Pressures  = (Nothing, Nothing, Nothing)
  , _gt2WarnThresh = (1050.0, 1050.0, 1050.0)
  , _gt2DevName    = Nothing
  , _gt2Label      = ("A", "B", "C")
  }

initMeasurement :: Measurements
initMeasurement = Measurements
 { _coldIon       = initColdIon
 , _lakeShore     = initLakeShore
 , _graphixThree1 = initGraphixThree1
 , _graphixThree2 = initGraphixThree2
 , _writeLog      = False
 , _onScreenInfo  = ["Initialising Programm ..."]
 , _plotInterval  = 60
 }


{- ########################################################################## -}
{- Main Part, performing IO and executing the interface -}
{- ########################################################################## -}
main :: IO ()
main = do
  -- possibly read the command line for the config file
  args <- getArgs

  -- if configFile exists and is valid, read it, otherwise use the internal
  -- defaults
  startMeasure <-
        if length args == 0
          then return initMeasurement
          else do
            confFile <- T.readFile (head args)
            let confMeasureParse = parseOnly defaultsParser confFile
            if isLeft confMeasureParse
              then do
                putStrLn "configFile is not valid, using defaults instead"
                return initMeasurement
              else return $ fromRight confMeasureParse

  -- make a second thread, that constantly feeds ticks into a channel, read by
  -- Brick, 50 ticks buffering maximal
  chan <- newBChan 50
  _ <- forkIO $ forever $ do
         writeBChan chan Tick
         threadDelay tickDistance

  -- execute the TUI with the startMeasure
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app startMeasure
