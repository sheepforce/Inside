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
type Pressure = Double
data ColdIon = ColdIon
  { _ciEnabled  :: Bool
  , _ciTask     :: CI.CICommand
  , _ciPort     :: FilePath
  , _ciChannel  :: Word8
  , _ciPressure :: Maybe Pressure
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

-- | The user interface mainly takes care of displaying things. Therefore
-- | the state of our app is only described by tasks for specific devices, that
-- | shall be carried out in the current loop
data Measurements = Measurements
  { _coldIon   :: ColdIon     -- infos about the Coldion CU-100
  , _lakeShore :: LakeShore -- infos about the LakeShore 335
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

-- | the user interface, composed of individual Widgets
drawUI :: Measurements -> [Widget Name]
drawUI m =
  [ BC.center  -- $(coldionWidgetPressure m) <=> (deviceWidgetColdIon m)]
  $ hBox [coldionWidgetPressure m, lakeShoreWidgetTemperature m]
  <=>
  hBox [deviceWidgetColdIon m, deviceWidgetLakeShore m]
  ]

{- ======= -}
{- ColdIon -}
{- ======= -}
-- | widget for holding the Coldion pressure
coldionWidgetPressure :: Measurements -> Widget Name
coldionWidgetPressure m =
  hLimit 20
  $ withBorderStyle BBS.unicodeBold
  $ BB.borderWithLabel (str shownDevName)
  $ BC.hCenter
  $ padAll 1
  $ str $ show shownPressure
  where
    shownDevName = fromMaybe "ColdIon CU-100" $ m ^. (coldIon . ciDevName)
    shownPressure = fromMaybe 0.0 $ m ^. (coldIon . ciPressure)

-- | showing the parameters of the devices (ports, ...)
deviceWidgetColdIon :: Measurements -> Widget Name
deviceWidgetColdIon m =
  hLimit 20
  $ withBorderStyle BBS.unicodeBold
  $ BB.borderWithLabel (str "ColdIon CU-100")
  $ padAll 1
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

{- ========= -}
{- LakeShore -}
{- ========= -}
lakeShoreWidgetTemperature :: Measurements -> Widget Name
lakeShoreWidgetTemperature m =
  hLimit 20
  $ withBorderStyle BBS.unicodeBold
  $ BB.borderWithLabel (str shownDevName)
  $ BC.hCenter
  $ padAll 1
  $ vBox
  [ str $ show $ fromMaybe 0.0 tempA
  , str $ show $ fromMaybe 0.0 tempB
  ]
  where
    shownDevName = fromMaybe "LakeShore 335" $ m ^. (lakeShore . lsDevName)
    maybeTemps = m ^. (lakeShore . lsTemperatures)
    tempA = fst maybeTemps
    tempB = snd maybeTemps

deviceWidgetLakeShore :: Measurements -> Widget Name
deviceWidgetLakeShore m =
  hLimit 20
  $ withBorderStyle BBS.unicodeBold
  $ BB.borderWithLabel (str "LakeShore 335")
  $ padAll 1
  $ str
  $  ""
  ++ "  Enabled:\n"
  ++ "    " ++ shownEnabled ++ "\n\n"
  ++ "  Port:\n"
  ++ "    " ++ shownPort ++ "\n\n"
  ++ "  Device Name:\n"
  ++ "    " ++ show shownDevName
  where
    shownEnabled = show $ m ^. (lakeShore . lsEnabled)
    shownPort = m ^. (lakeShore . lsPort)
    shownDevName = m ^. (lakeShore . lsDevName)

{- ########################################################################## -}
{- Define EventHandling -}
{- ########################################################################## -}
-- | handling events that occur. These are the ticks, fed into the app as well
-- | as the keys pressed
handleEvent :: Measurements -> BrickEvent Name Tick -> EventM Name (Next Measurements)
handleEvent m (AppEvent Tick)                       = liftIO (getCurrentConditions m) >>= continue
handleEvent m (VtyEvent (V.EvKey (V.KChar 'c') [])) = continue $ toggleColdIon m
handleEvent m (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ toggleLakeShore m
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
  return $
    m
    & coldIon . ciPressure .~ newColdIonPressure             -- write pressure of ColdIon
    & lakeShore . lsTemperatures .~ newLakeShoreTemperaturss -- write Temperatures of LS

{- ======= -}
{- ColdIon -}
{- ======= -}
-- | Sending, receiving and in die gasphase 😀 und die meisten gruppen, kenn ich ja schon, hab ich ja reingeschmissenunderstanding an request to the ColdIon.
-- | The pressure is retured
coldIonPressureUpdate :: Measurements -> IO (Maybe Double)
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
      let ciDevName =
            AB.parseOnly CI.parseName
            $ B.pack
            . CI.dataBytes2List
            . CI._ci_data
            $ fromRight coldIonAnswerCIString

      if isRight ciDevName
        then return $ Just (fromRight ciDevName)
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
  { _ciEnabled = False
  , _ciTask = CI.GetDevName
  , _ciPort = "/dev/ttyUSB0"
  , _ciChannel = 1
  , _ciPressure = Nothing
  , _ciDevName = Nothing
  }

initLakeShore :: LakeShore
initLakeShore = LakeShore
  { _lsEnabled = False
  , _lsTask = LS.AskTemperature LS.A
  , _lsPort = "/dev/ttyUSB1"
  , _lsTemperatures = (Nothing, Nothing)
  , _lsDevName = Nothing
  }

initMeasurement :: Measurements
initMeasurement = Measurements
 { _coldIon = initColdIon
 , _lakeShore = initLakeShore
 }


{- ########################################################################## -}
{- Main Part, performing IO and executing the interface -}
{- ########################################################################## -}
main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 2500000

  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app initMeasurement
