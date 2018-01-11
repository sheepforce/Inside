{-# LANGUAGE TemplateHaskell #-}

module Internal.UI.Data
( Tick(..)
, Name
, CIPressure
, ColdIon(..)
, ciEnabled
, ciTask
, ciPort
, ciChannel
, ciPressure
, ciWarnThresh
, ciDevName
, ciLabel
, LSTemperatures
, LakeShore(..)
, lsEnabled
, lsTask
, lsPort
, lsTemperatures
, lsWarnThresh
, lsDevName
, lsLabel
, GT1Pressures
, GraphixThree1(..)
, gt1Enabled
, gt1Task
, gt1Port
, gt1Pressures
, gt1WarnThresh
, gt1DevName
, gt1Label
, GT2Pressures
, GraphixThree2(..)
, gt2Enabled
, gt2Task
, gt2Port
, gt2Pressures
, gt2WarnThresh
, gt2DevName
, gt2Label
, Measurements(..)
, coldIon
, lakeShore
, graphixThree1
, graphixThree2
, writeLog
, onScreenInfo
, plotInterval
, Warning(..)
, initColdIon
, initLakeShore
, initGraphixThree1
, initGraphixThree2
, initMeasurement
) where
import           Data.Word
import qualified Hardware.LakeShore.TemperatureController as LS
import qualified Hardware.Leybold.GraphixThree            as GT
import qualified Hardware.Vacom.Coldion                   as CI
import           Lens.Micro.TH

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

-- | three different types of warning states used for device visualisation in
-- | the program
data Warning =
    OK
  | OverThresh
  | Invalid
  deriving (Show, Eq)

-- | defaults for the UI
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
