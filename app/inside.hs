{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Brick
import           Brick.BChan                (newBChan, writeBChan)
import qualified Brick.Widgets.Border       as BB
import qualified Brick.Widgets.Border.Style as BBS
import qualified Brick.Widgets.Center       as BC
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever, void)
import           Data.Maybe
import           Data.Word
import qualified Graphics.Vty               as V
import qualified Hardware.Vacom.Coldion     as CI
import           Lens.Micro
import           Lens.Micro.TH
import System.Hardware.Serialport
import qualified Data.ByteString                  as B
import           Data.Attoparsec.ByteString.Char8
import           Data.Attoparsec.ByteString.Lazy
import Data.Either.Unwrap
import Control.Monad.IO.Class


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
  { _task     :: CI.CICommand
  , _port     :: FilePath
  , _channel  :: Word8
  , _pressure :: Maybe Pressure
  , _devName  :: Maybe String
  } deriving (Show)
makeLenses ''ColdIon

-- | The user interface mainly takes care of displaying things. Therefore
-- | the state of our app is only described by tasks for specific devices, that
-- | shall be carried out in the current loop
data Measurements = Measurements
  { _coldIon :: ColdIon -- what is the Coldion CU-100 supposed to do?
  } deriving (Show)
makeLenses ''Measurements




{- ########################################################################## -}
{- Define the user interface -}
{- ########################################################################## -}
-- | defining the Brick app type
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
drawUI m = [ BC.center $ (coldionWidgetPressure m) <=> (deviceWidgetColdIon m)]

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
    shownDevName = fromMaybe "ColdIon CU-100" $ m ^. (coldIon . devName)
    shownPressure = fromMaybe 0.0 $ m ^. (coldIon . pressure)

-- | showing the parameters of the devices (ports, ...)
deviceWidgetColdIon :: Measurements -> Widget Name
deviceWidgetColdIon m =
  hLimit 50
  $ withBorderStyle BBS.unicodeBold
  $ BB.borderWithLabel (str "Device Configuration")
  $ padAll 2
  $ str
  $  "ColdIon CU-100\n\n"
  ++ "  Port:\n"
  ++ "    " ++ shownPort

  where
    shownPort = show $ m ^. (coldIon . port)


-- | handling events that occur. These are the ticks, fed into the app as well
-- | as the keys pressed
handleEvent :: Measurements -> BrickEvent Name Tick -> EventM Name (Next Measurements)
handleEvent m (AppEvent Tick)                = liftIO (getCurrentConditions m) >>= continue
handleEvent m (VtyEvent (V.EvKey V.KEsc [])) = halt m

-- | IO function
getCurrentConditions :: Measurements -> IO Measurements
getCurrentConditions m = do
  newColdIonPressure <- coldIonPressureUpdate m  -- update pressure of ColdIon

  return $
    m & coldIon . pressure .~ newColdIonPressure -- write pressure of ColdIon

-- | sending, receiving and understanding an request to the ColdIon.
-- | port of the ColdIon comes from the Measurement
-- | this is a dangerous function. which can fail badly
coldIonPressureUpdate :: Measurements -> IO (Maybe Double)
coldIonPressureUpdate m = do
  ci <- openSerial coldIonPort defaultSerialSettings { commSpeed = CS19200 }
  if isNothing ciRequest
    then return Nothing
    else do
      send ci $ CI.ciString2ByteString . fromJust $ ciRequest
      ciAnswer <- recv ci 24

      -- This is perfectly safe function returning Nothing if it fails.
      -- nevertheless; it is an IO function, therefore we have IO type
      let ciAnswerCIString = parseOnly CI.parseAnswer ciAnswer

      if isRight ciAnswerCIString
        then do
          let ciPressure =
                parseOnly CI.parsePressure
                $ B.pack
                . CI.dataBytes2List
                . CI._ci_data
                $ fromRight ciAnswerCIString

          if (isRight ciPressure)
            then return $ Just (fromRight ciPressure)
            else return Nothing
        else return Nothing

  where
    coldIonPort = m ^. coldIon . port
    ciChannel = m ^. coldIon . channel
    ciRequest = CI.createCommandCIString (CI.AskPressure ciChannel)



theMeasurements :: AttrMap
theMeasurements = attrMap V.defAttr [("pressureAttr" :: AttrName, V.red `on` V.blue)]

initColdIon = ColdIon
  { _task = CI.GetDevName
  , _port = "/dev/ttyUSB0"
  , _channel = 0x01
  , _pressure = Nothing
  , _devName = Nothing
  }

initMeasurement = Measurements
 { _coldIon = initColdIon
 }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 1000000



  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app initMeasurement
