{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Brick
import           Brick.BChan                              (newBChan, writeBChan)
import qualified Brick.Widgets.Border                     as BB
import qualified Brick.Widgets.Border.Style               as BBS
import qualified Brick.Widgets.Center                     as BC
import           Control.Applicative
import           Control.Concurrent                       (forkIO, threadDelay)
import           Control.Monad                            (forever, void)
import           Control.Monad.IO.Class
import qualified Data.Attoparsec.ByteString.Lazy          as AB
import           Data.Attoparsec.Text.Lazy
import qualified Data.ByteString                          as B
import           Data.Either.Unwrap
import           Data.Maybe
import qualified Data.Text                                as T
import qualified Data.Text.IO                             as T
import           Data.Word
import qualified Graphics.Vty                             as V
import qualified Hardware.LakeShore.TemperatureController as LS
import qualified Hardware.Leybold.GraphixThree            as GT
import qualified Hardware.Vacom.Coldion                   as CI
import           Lens.Micro
import           Lens.Micro.TH
import           System.Hardware.Serialport


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
type CIPressure = Double
data ColdIon = ColdIon
  { _ciEnabled  :: Bool
  , _ciTask     :: CI.CICommand
  , _ciPort     :: FilePath
  , _ciChannel  :: Word8
  , _ciPressure :: Maybe CIPressure
  , _ciDevName  :: Maybe String
  } deriving (Show)
makeLenses ''ColdIon

-- | Ther LakeShore 335 interface. What it should do and on which port (something
-- | like /dev/ttyUSB0) the device is connected
type Temperatures = (Maybe Double, Maybe Double)
data LakeShore = LakeShore
  { _lsEnabled      :: Bool
  , _lsTask         :: LS.LSCommand
  , _lsPort         :: FilePath
  , _lsTemperatures :: Temperatures
  , _lsDevName      :: Maybe String
  } deriving (Show)
makeLenses ''LakeShore

-- | The firtst of the to GraphixThree1 controllers, 3 gauges
type GT1Pressures = (Maybe Double, Maybe Double, Maybe Double)
data GraphixThree1 = GraphixThree1
  { _gt1Enabled   :: Bool
  , _gt1Task      :: GT.GTCommand
  , _gt1Port      :: FilePath
  , _gt1Pressures :: GT1Pressures
  , _gt1DevName   :: Maybe String
  } deriving (Show)
makeLenses '' GraphixThree1

-- | The second of the to GraphixThree1 controllers, 3 gauges
type GT2Pressures = (Maybe Double, Maybe Double, Maybe Double)
data GraphixThree2 = GraphixThree2
  { _gt2Enabled   :: Bool
  , _gt2Task      :: GT.GTCommand
  , _gt2Port      :: FilePath
  , _gt2Pressures :: GT2Pressures
  , _gt2DevName   :: Maybe String
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
  } deriving (Show)
makeLenses ''Measurements


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
hWidgetBoxSize = 25

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
  ]
  {-
  [ BC.center
  $ hBox [coldionWidgetPressure m, lakeShoreWidgetTemperature m, graphixThree1WidgetPressure m, graphixThree2WidgetPressure m]
  <=>
  hBox [deviceWidgetColdIon m, deviceWidgetLakeShore m, deviceWidgetGraphixThree1 m, deviceWidgetGraphixThree2 m]
  <=>
  hBox [coldIonWarningWidget m, lakeShoreWarningWidget m, graphixThree1WarningWidget m, graphixThree2WarningWidget m]
  ]
  -}

{- ======= -}
{- ColdIon -}
{- ======= -}
-- | widget for holding the Coldion pressure
coldionWidgetPressure :: Measurements -> Widget Name
coldionWidgetPressure m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ str $ "\n" ++ show shownPressure ++ "\n\n"
  where
    shownDevName = fromMaybe "ColdIon CU-100" $ m ^. (coldIon . ciDevName)
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
  ++ "    " ++ show shownDevName

  where
    shownEnabled = show $ m ^. (coldIon . ciEnabled)
    shownPort = m ^. (coldIon . ciPort)
    shownChannel = show $ m ^. (coldIon . ciChannel)
    shownDevName = m ^. (coldIon . ciDevName)

coldIonWarningWidget :: Measurements -> Widget Name
coldIonWarningWidget m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ if (fromMaybe 0.0 (m ^. coldIon . ciPressure) >= 1.0e-10)
      then str "WARNING"
      else str "  OK   "

{- ========= -}
{- LakeShore -}
{- ========= -}
lakeShoreWidgetTemperature :: Measurements -> Widget Name
lakeShoreWidgetTemperature m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ vBox
  [ str $ show $ fromMaybe 0.0 tempA
  , str $ show $ fromMaybe 0.0 tempB
  , str "\n"
  ]
  where
    maybeTemps = m ^. (lakeShore . lsTemperatures)
    tempA = fst maybeTemps
    tempB = snd maybeTemps

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
  ++ "    " ++ show shownDevName
  where
    shownEnabled = show $ m ^. (lakeShore . lsEnabled)
    shownPort = m ^. (lakeShore . lsPort)
    shownDevName = m ^. (lakeShore . lsDevName)

lakeShoreWarningWidget :: Measurements -> Widget Name
lakeShoreWarningWidget m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ if (  fromMaybe 0.0 (fst $ m ^. lakeShore . lsTemperatures) >= 300.0
       || fromMaybe 0.0 (snd $ m ^. lakeShore . lsTemperatures) >= 300.0 )
      then str "WARNING"
      else str "  OK   "

{- ============== -}
{- GraphixThree 1 -}
{- ============== -}
graphixThree1WidgetPressure :: Measurements -> Widget Name
graphixThree1WidgetPressure m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ vBox
  [ str $ show $ fromMaybe 0.0 pressureA
  , str $ show $ fromMaybe 0.0 pressureB
  , str $ show $ fromMaybe 0.0 pressureC
  ]
  where
    shownDevName = fromMaybe "GraphixThree (1)" $ m ^. (graphixThree1 . gt1DevName)
    maybePressures = m ^. (graphixThree1 . gt1Pressures)
    pressureA = maybePressures ^. _1
    pressureB = maybePressures ^. _2
    pressureC = maybePressures ^. _3

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
  ++ "    " ++ show shownDevName
  where
    shownEnabled = show $ m ^. (graphixThree1 . gt1Enabled)
    shownPort = m ^. (graphixThree1 . gt1Port)
    shownDevName = m ^. (graphixThree1 . gt1DevName)

graphixThree1WarningWidget :: Measurements -> Widget Name
graphixThree1WarningWidget m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ if (  fromMaybe 0.0 (m ^. graphixThree1 . gt1Pressures . _1) >= 300.0
       || fromMaybe 0.0 (m ^. graphixThree1 . gt1Pressures . _2) >= 300.0
       || fromMaybe 0.0 (m ^. graphixThree1 . gt1Pressures . _3) >= 300.0 )
      then str "WARNING"
      else str "  OK   "

{- ============== -}
{- GraphixThree 2 -}
{- ============== -}
graphixThree2WidgetPressure :: Measurements -> Widget Name
graphixThree2WidgetPressure m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ vBox
  [ str $ show $ fromMaybe 0.0 pressureA
  , str $ show $ fromMaybe 0.0 pressureB
  , str $ show $ fromMaybe 0.0 pressureC
  ]
  where
    shownDevName = fromMaybe "GraphixThree (2)" $ m ^. (graphixThree2 . gt2DevName)
    maybePressures = m ^. (graphixThree2 . gt2Pressures)
    pressureA = maybePressures ^. _1
    pressureB = maybePressures ^. _2
    pressureC = maybePressures ^. _3

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
  ++ "    " ++ show shownDevName
  where
    shownEnabled = show $ m ^. (graphixThree2 . gt2Enabled)
    shownPort = m ^. (graphixThree2 . gt2Port)
    shownDevName = m ^. (graphixThree2 . gt2DevName)

graphixThree2WarningWidget :: Measurements -> Widget Name
graphixThree2WarningWidget m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ if (  fromMaybe 0.0 (m ^. graphixThree2 . gt2Pressures . _1) >= 300.0
       || fromMaybe 0.0 (m ^. graphixThree2 . gt2Pressures . _2) >= 300.0
       || fromMaybe 0.0 (m ^. graphixThree2 . gt2Pressures . _3) >= 300.0 )
      then str "WARNING"
      else str "  OK   "


{- ########################################################################## -}
{- Define EventHandling -}
{- ########################################################################## -}
-- | handling events that occur. These are the ticks, fed into the app as well
-- | as the keys pressed
handleEvent :: Measurements -> BrickEvent Name Tick -> EventM Name (Next Measurements)
handleEvent m (AppEvent Tick)                       = liftIO (getCurrentConditions m) >>= continue
handleEvent m (VtyEvent (V.EvKey (V.KChar 'c') [])) = continue $ toggleColdIon m
handleEvent m (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ toggleLakeShore m
handleEvent m (VtyEvent (V.EvKey (V.KChar 'g') [])) = continue $ toggleGraphixThree1 m
handleEvent m (VtyEvent (V.EvKey (V.KChar 't') [])) = continue $ toggleGraphixThree2 m
handleEvent m (VtyEvent (V.EvKey V.KEsc []))        = halt m
handleEvent m _                                     = continue m

-- | Request an update of all currently connected devices.
-- | This is basically a wrapper function around all the individual update
-- | function for the devices
getCurrentConditions :: Measurements -> IO Measurements
getCurrentConditions m = do
  newColdIonPressure <-                      -- update ColdIon Pressure
    if (m ^. coldIon . ciEnabled)            -- if enabled
      then coldIonPressureUpdate m           -- by asking the device
      else return Nothing                    -- disbaled -> Nothing
  newLakeShoreTemperaturss <-                -- update LakeShore temperatures
    if (m ^. lakeShore . lsEnabled)          -- if enabled
      then lakeShoreTempUpdate m
      else return (Nothing, Nothing)
  newGraphixThree1Pressures <-
    if (m ^. graphixThree1 . gt1Enabled)
      then graphixThree1PressureUpdate m
      else return (Nothing, Nothing, Nothing)
  newGraphixThree2Pressures <-
    if (m ^. graphixThree2 . gt2Enabled)
      then graphixThree2PressureUpdate m
      else return (Nothing, Nothing, Nothing)
  return $
    m
    & coldIon . ciPressure .~ newColdIonPressure                -- write pressure of ColdIon
    & lakeShore . lsTemperatures .~ newLakeShoreTemperaturss    -- write Temperatures of LS
    & graphixThree1 . gt1Pressures .~ newGraphixThree1Pressures -- write Pressures of GT1
    & graphixThree2 . gt2Pressures .~ newGraphixThree2Pressures -- write Pressures of GT2

{- ======= -}
{- ColdIon -}
{- ======= -}
-- | Sending, receiving and understanding an request to the ColdIon.
-- | The pressure is retured
coldIonPressureUpdate :: Measurements -> IO (Maybe CIPressure)
coldIonPressureUpdate m = do
  ci <- openSerial coldIonPort defaultSerialSettings { commSpeed = CS19200 }
  _ <- send ci $ CI.ciString2ByteString . fromJust $ coldIonRequest
  coldIonAnswer <- recv ci 24
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
        then return $ Just (fromRight coldIonPressure)
        else return Nothing
    else return Nothing
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
theMeasurements = attrMap V.defAttr [("pressureAttr" :: AttrName, V.red `on` V.blue)]


{- ########################################################################## -}
{- Parsers -}
{- ########################################################################## -}
-- | parsing the defaults from a file and using defaults, that are not parsed
-- | from the standard definition
defaultsParser :: Parser Measurements
defaultsParser = do
  defColdIon <- coldIonParser
  return $
    initMeasurement
    & coldIon .~ defColdIon

-- | parse the ColdIon block and update the defaults with the parsed result
coldIonParser :: Parser ColdIon
coldIonParser = do
  _ <- string $ T.pack "[ColdIon]"
  skipSpace
  _ <- string $ T.pack "enabled ="
  skipSpace
  enabledP <- (string $ T.pack "True") <|> (string $ T.pack "False")
  _ <- string $ T.pack "port ="
  skipSpace
  portP <- manyTill anyChar endOfLine
  skipSpace
  _ <- string $ T.pack "channel ="
  channelP <- decimal

  return $
    initColdIon
    & ciEnabled .~ (enabledP == T.pack "True")
    & ciPort .~ portP
    & ciChannel .~ channelP

lakeShoreParser :: Parser LakeShore
lakeShoreParser = do
  _ <- string $ T.pack "[LakeShore355]"
  skipSpace
  _ <- string $ T.pack "enabled ="
  skipSpace
  enabledP <- (string $ T.pack "True") <|> (string $ T.pack "False")
  _ <- string $ T.pack "port ="
  skipSpace
  portP <- manyTill anyChar endOfLine

  return $
    initLakeShore
    & lsEnabled .~ (enabledP == T.pack "True")
    & lsPort .~ portP


{- ########################################################################## -}
{- Defaults for the Measurement State -}
{- ########################################################################## -}
initColdIon :: ColdIon
initColdIon = ColdIon
  { _ciEnabled  = False
  , _ciTask     = CI.GetDevName
  , _ciPort     = "/dev/ttyUSB0"
  , _ciChannel  = 1
  , _ciPressure = Nothing
  , _ciDevName  = Nothing
  }

initLakeShore :: LakeShore
initLakeShore = LakeShore
  { _lsEnabled      = False
  , _lsTask         = LS.AskTemperature LS.A
  , _lsPort         = "/dev/ttyUSB1"
  , _lsTemperatures = (Nothing, Nothing)
  , _lsDevName      = Nothing
  }

initGraphixThree1 :: GraphixThree1
initGraphixThree1 = GraphixThree1
  { _gt1Enabled   = False
  , _gt1Task      = GT.AskPressure GT.A
  , _gt1Port      = "/dev/ttyUSB2"
  , _gt1Pressures = (Nothing, Nothing, Nothing)
  , _gt1DevName   = Nothing
  }

initGraphixThree2 :: GraphixThree2
initGraphixThree2 = GraphixThree2
  { _gt2Enabled   = False
  , _gt2Task      = GT.AskPressure GT.A
  , _gt2Port      = "/dev/ttyUSB3"
  , _gt2Pressures = (Nothing, Nothing, Nothing)
  , _gt2DevName   = Nothing
  }

initMeasurement :: Measurements
initMeasurement = Measurements
 { _coldIon   = initColdIon
 , _lakeShore = initLakeShore
 , _graphixThree1 = initGraphixThree1
 , _graphixThree2 = initGraphixThree2
 }


{- ########################################################################## -}
{- Main Part, performing IO and executing the interface -}
{- ########################################################################## -}
main :: IO ()
main = do
  chan <- newBChan 50
  _ <- forkIO $ forever $ do
         writeBChan chan Tick
         threadDelay 2500000

  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app initMeasurement
