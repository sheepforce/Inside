module Internal.UI.EventHandling
( tickDistance
, handleEvent
, getCurrentConditions
, logWriter
, toggleLogging
, plotter
, plotterHandler
, incrPlotInterval
, decrPlotInterval
, coldIonPressureUpdate
, coldIonNameUpdate
, toggleColdIon
, toggleLakeShore
, graphixThree1PressureUpdate
, toggleGraphixThree1
, graphixThree2PressureUpdate
, toggleGraphixThree2
) where
import           Control.Concurrent                       (threadDelay)
import           Control.Exception                        (catch)
import           Data.Attoparsec.ByteString.Lazy          hiding (take)
import qualified Data.Attoparsec.Text.Lazy                as AT
import qualified Data.ByteString                          as B
import           Data.Either.Unwrap
import           Data.Maybe
import qualified Data.Text.IO                             as T
import           Data.Time
import qualified Hardware.LakeShore.TemperatureController as LS
import qualified Hardware.Leybold.GraphixThree            as GT
import qualified Hardware.Vacom.Coldion                   as CI
import qualified Internal.Plotting                        as P
import           Internal.UI.Data
import           Lens.Micro
import           System.Directory
import           System.Hardware.Serialport
import           System.IO
import           Text.Printf
import qualified Graphics.Vty                             as V
import           Brick
import           Control.Monad.IO.Class
import           Text.Read
import qualified Graphics.Rendering.Chart.Backend.Cairo   as Cairo


-- | delay in seconds before new tick is fed into the TUI (micro seconds)
tickDistance :: Int
tickDistance = 2500000

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

-- | Request an update of all currently connected devices.
-- | This is basically a wrapper function around all the individual update
-- | function for the devices
getCurrentConditions :: Measurements -> IO Measurements
getCurrentConditions m = do
  let mOnScreenInfoReset = m & onScreenInfo .~ ["Waiting ..."]
  newColdIonMeasurements <-
    if (m ^. coldIon . ciEnabled)
      then catch (updateColdIonPressure mOnScreenInfoReset) (ciHandler mOnScreenInfoReset)
      else return $ mOnScreenInfoReset & coldIon . ciPressure .~ Nothing

  newLakeShoreMeasurements <-
    if (m ^. lakeShore . lsEnabled)
      then catch (updateLakeShoreTemperatures mOnScreenInfoReset) (lsHandler mOnScreenInfoReset)
      else return $ mOnScreenInfoReset & lakeShore . lsTemperatures .~ (Nothing, Nothing)

  newGraphixThree1Measurements <-
    if (m ^. graphixThree1 . gt1Enabled)
      then catch (updateGraphixThree1Pressures mOnScreenInfoReset) (gt1Handler mOnScreenInfoReset)
      else return $ mOnScreenInfoReset & graphixThree1 . gt1Pressures .~ (Nothing, Nothing, Nothing)

  newGraphixThree2Measurements <-
    if (mOnScreenInfoReset ^. graphixThree2 . gt2Enabled)
      then catch (updateGraphixThree2Pressures mOnScreenInfoReset) (gt2Handler mOnScreenInfoReset)
      else return $ mOnScreenInfoReset & graphixThree2 . gt2Pressures .~ (Nothing, Nothing, Nothing)

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
plotter device m  = do
  -- get the current date to construct the name of the current log file, so that
  -- only the most recent will be read
  logDate <- do
    today <- getZonedTime
    return $ localDay . zonedTimeToLocalTime $ today
  let logName = "inside_" ++ show logDate ++ ".log"

  -- read and parse the log file
  logCont <- T.readFile logName
  let latestLogP = AT.parseOnly P.parsePlot logCont

  -- if parsing suceeded, unwrap the log informations and check if enough data
  -- the plot are present
  if isRight latestLogP
    then do
      let latestLog = fromRight latestLogP
          takeable = length latestLog > linesToTake

      if takeable
        then do
          let logInInterval = take linesToTake latestLog
          mapM_ (\d -> P.plotSelectedLogData d logInInterval Cairo.PNG (devFileName d)) device
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
      | d == P.ColdIon = "ColdIonPlot.png"
      | d == P.LakeShore LS.A = "LakeShore_A.png"
      | d == P.LakeShore LS.B = "LakeShore_B.png"
      | d == P.GraphixThree1 GT.A = "GraphixThree1_A.png"
      | d == P.GraphixThree1 GT.B = "GraphixThree1_B.png"
      | d == P.GraphixThree1 GT.C = "GraphixThree1_C.png"
      | d == P.GraphixThree2 GT.A = "GraphixThree2_A.png"
      | d == P.GraphixThree2 GT.B = "GraphixThree2_B.png"
      | d == P.GraphixThree2 GT.C = "GraphixThree2_C.png"
      | otherwise = "IHaveNoIdeaWhatIAmDoingHere.png"
    oldOnScreenInfo = m ^. onScreenInfo

    -- | handling plotting that is going wrong
plotterHandler :: Measurements -> IOError -> IO Measurements
plotterHandler m e = return $
  m & onScreenInfo .~ oldOnScreenInfo ++ ["Plotting : FAILED with " ++ show e]
  where
    oldOnScreenInfo = m ^. onScreenInfo

-- | increment the plot interval by 1 minute
incrPlotInterval :: Measurements -> Measurements
incrPlotInterval m = m & plotInterval .~ (oldInterval + 1)
  where
    oldInterval = m ^. plotInterval

-- | decrement the plot interval by 1 minute
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
  let coldIonAnswerCIString = parseOnly CI.parseAnswer coldIonAnswer

  if isRight coldIonAnswerCIString
    then do
      let coldIonPressure =
            parseOnly CI.parsePressure
            $ B.pack
            . CI.dataBytes2List
            . CI._ciData
            $ fromRight coldIonAnswerCIString

      if isRight coldIonPressure
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
  let coldIonAnswerCIString = parseOnly CI.parseAnswer coldIonAnswer
  if isRight coldIonAnswerCIString
    then do
      let coldIonDevName =
            parseOnly CI.parseName
            $ B.pack
            . CI.dataBytes2List
            . CI._ciData
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
-- | request current conditions from the LakeShore
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

  let lakeShoreTempA = parseOnly LS.parseTemperature lakeShoreAnswerA
      lakeShoreTempB = parseOnly LS.parseTemperature lakeShoreAnswerB

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

-- | toggle the lakeShore
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
  -- request pressure from gauge 1
  gt1A <- openSerial graphixThree1Port defaultSerialSettings { commSpeed = CS38400 }
  _ <- send gt1A $ fromJust graphixThreeRequestA
  threadDelay 10000
  graphixThree1AnswerA <- recv gt1A 255
  closeSerial gt1A

  -- request pressure from gauge 2
  threadDelay 240000
  gt1B <- openSerial graphixThree1Port defaultSerialSettings { commSpeed = CS38400 }
  _ <- send gt1B $ fromJust graphixThreeRequestB
  threadDelay 10000
  graphixThree1AnswerB <- recv gt1B 255
  closeSerial gt1B

  -- request pressure from gauge 3
  threadDelay 240000
  gt1C <- openSerial graphixThree1Port defaultSerialSettings { commSpeed = CS38400 }
  _ <- send gt1C $ fromJust graphixThreeRequestC
  threadDelay 10000
  graphixThree1AnswerC <- recv gt1C 255
  closeSerial gt1C

  let graphixThree1TempA = parseOnly GT.parsePressure graphixThree1AnswerA
      graphixThree1TempB = parseOnly GT.parsePressure graphixThree1AnswerB
      graphixThree1TempC = parseOnly GT.parsePressure graphixThree1AnswerC

  if isLeft graphixThree1TempA
    then
      if isLeft graphixThree1TempB
        then
          if isLeft graphixThree1TempC
            then return (Nothing, Nothing, Nothing)
            else return (Nothing, Nothing, Just $ fromRight graphixThree1TempC)
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
-- | update all three readings from the GraphixThree (Number 2)
-- | if a gauge is not connected or transmitting data fails,
-- | a Nothing is returned
graphixThree2PressureUpdate :: Measurements -> IO (Maybe Double, Maybe Double, Maybe Double)
graphixThree2PressureUpdate m = do
  gt2A <- openSerial graphixThree2Port defaultSerialSettings { commSpeed = CS38400 }
  _ <- send gt2A $ fromJust graphixThreeRequestA
  threadDelay 10000
  graphixThree2AnswerA <- recv gt2A 255
  closeSerial gt2A

  threadDelay 240000
  gt2B <- openSerial graphixThree2Port defaultSerialSettings { commSpeed = CS38400 }
  _ <- send gt2B $ fromJust graphixThreeRequestB
  threadDelay 10000
  graphixThree2AnswerB <- recv gt2B 255
  closeSerial gt2B

  threadDelay 240000
  gt2C <- openSerial graphixThree2Port defaultSerialSettings { commSpeed = CS38400 }
  _ <- send gt2C $ fromJust graphixThreeRequestC
  threadDelay 10000
  graphixThree2AnswerC <- recv gt2C 255
  closeSerial gt2C

  let graphixThree2TempA = parseOnly GT.parsePressure graphixThree2AnswerA
      graphixThree2TempB = parseOnly GT.parsePressure graphixThree2AnswerB
      graphixThree2TempC = parseOnly GT.parsePressure graphixThree2AnswerC

  if isLeft graphixThree2TempA
    then
      if isLeft graphixThree2TempB
        then
          if isLeft graphixThree2TempC
            then return (Nothing, Nothing, Nothing)
            else return (Nothing, Nothing, Just $ fromRight graphixThree2TempC)
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
