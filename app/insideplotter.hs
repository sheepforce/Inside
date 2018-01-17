{-# LANGUAGE DeriveDataTypeable #-}

import           Control.Concurrent
import           Control.Exception                        (catch)
import           Control.Monad
import           Data.Attoparsec.Text.Lazy
import           Data.Either.Unwrap
import qualified Data.Text                                as T
import qualified Data.Text.IO                             as T
import           Data.Time
import qualified Hardware.LakeShore.TemperatureController as LS
import qualified Hardware.Leybold.GraphixThree            as GT
import qualified Internal.Plotting                        as P
import           System.Console.CmdArgs
import qualified Graphics.Rendering.Chart.Backend.Cairo   as Cairo
import Lens.Micro

{- ########################################################################## -}
{- CmdArgs                                                                    -}
{- ########################################################################## -}
-- | define the command line arguments data type
data PlotOpts = PlotOpts
 { logPath        :: String
 , webPath        :: String
 , coldIon        :: Bool
 , lakeShore      :: Bool
 , graphixThree1  :: Bool
 , graphixThree2  :: Bool
 , updateInterval :: Int
 , plotInterval   :: Int
 } deriving (Show, Data, Typeable)

-- | command line arguments defaults
plotOpts = PlotOpts
  { logPath        = "inside"     &= help "directory of the log files"                       &= typ "DIRECTORY"
  , webPath        = "inside-web" &= help "directory of the web contents"                    &= typ "DIRECTORY"
  , coldIon        = False        &= help "switch to not plot graph for the ColdIon"         &= typ "BOOL"
  , lakeShore      = False        &= help "switch to not plot graphs for the LakeShore"      &= typ "BOOL"
  , graphixThree1  = False        &= help "switch to not plot graphs for the GraphixThree 1" &= typ "BOOL"
  , graphixThree2  = False        &= help "switch to not plot graphs for the GraphixThree 2" &= typ "BOOL"
  , updateInterval = 5000000      &= help "delay between updating plots"                     &= typ "INT"
  , plotInterval   = 60           &= help "history in minutes to plot"                       &= typ "INT"
  }
mode = cmdArgsMode plotOpts


{- ########################################################################## -}
{- global functions -}
{- ########################################################################## -}
-- | handler for IO errors while reading log files
logReadHandler :: IOError -> IO T.Text
logReadHandler err = do
  putStrLn $ "could not read log file with " ++ show err
  putStrLn "returning empty log file instead"
  return $ T.pack " "


{- ########################################################################## -}
{- main -}
{- ########################################################################## -}
-- | main module
main :: IO ()
main = do
  -- get the arguments from the command line
  arguments <- cmdArgs plotOpts
  let logpath = logPath arguments
      webpath = webPath arguments
      ciSw = not $ coldIon arguments
      lsSw = not $ lakeShore arguments
      gt1Sw = not $ graphixThree1 arguments
      gt2Sw = not $ graphixThree2 arguments
      currentUpdateInterval = updateInterval arguments
      currentPlotInterval = plotInterval arguments

  -- periodically execute parsing and plotting
  forever $ do
    -- ask for the current day and build the name of the log file of the current day
    now1 <- getZonedTime
    putStrLn $ show now1 ++ " : starting update cycle"

    logDate <- do
      today <- getZonedTime
      return $ localDay . zonedTimeToLocalTime $ today
    let insideLogName = "inside_" ++ show logDate ++ ".log"
        insideLogPath = logpath ++ "/" ++ insideLogName
        oldLogDate = addDays (-1) logDate
        insideOldLogName = "inside_" ++ show oldLogDate ++ ".log"
        insideOldLogPath = logpath ++ "/" ++ insideOldLogName

    now2 <- getZonedTime
    putStrLn $ show now2 ++ " : reading in log files with names: " ++ insideLogPath ++ " , " ++ insideOldLogPath
    -- read the log files
    logContent <- catch (T.readFile insideLogPath) logReadHandler
    oldLogContent <- catch (T.readFile insideOldLogPath) logReadHandler

    -- try parsing the log files and combining the results but only the lines
    -- tags are only read from todays log file
    let logP = parseOnly P.parsePlot logContent
        oldLogP = parseOnly P.parsePlot oldLogContent
        logLinesToday
          | isRight logP = P._plotDats $ fromRight logP
          | otherwise = []
        logLinesYesterday
          | isRight oldLogP = P._plotDats $ fromRight oldLogP
          | otherwise = []
        completeLogLines = logLinesYesterday ++ logLinesToday

        -- number of lines to read from the plot file
        insideInterval = 2.5 -- delay between updates in the main app in seconds
        linesPerMinute = (round :: Double -> Int) $ 60.0 / insideInterval
        linesToTake = currentPlotInterval * linesPerMinute

        -- the relevant part of the log file for the given time to be plotted
        interestingLogLines = drop (length completeLogLines - linesToTake) completeLogLines

        -- this includes the combined logs from today and yesterday but only the
        -- names from todays log. Ignore if parsing worked because this is only
        -- requested if enough lines are present and this is already checked
        interestingLog = P.PlotData
          { P._ciTag    = fromRight logP ^. P.ciTag
          , P._lsTags   = fromRight logP ^. P.lsTags
          , P._gt1Tags  = fromRight logP ^. P.gt1Tags
          , P._gt2Tags  = fromRight logP ^. P.gt2Tags
          , P._plotDats = interestingLogLines
          }

    if (length interestingLogLines >= linesToTake)
      then do
        when ciSw $ do
          P.plotSelectedLogData P.ColdIon interestingLog Cairo.PNG (webpath ++ "/" ++ "coldion.png")
          now3 <- getZonedTime
          putStrLn $ show now3 ++ " : plotted ColdIon data"

        when lsSw $ do
          P.plotSelectedLogData (P.LakeShore LS.A) interestingLog Cairo.PNG (webpath ++ "/" ++ "lakeshorea.png")
          P.plotSelectedLogData (P.LakeShore LS.B) interestingLog Cairo.PNG (webpath ++ "/" ++ "lakeshoreb.png")
          now4 <- getZonedTime
          putStrLn $ show now4 ++ " : plotted LakeShore data"

        when gt1Sw $ do
          P.plotSelectedLogData (P.GraphixThree1 GT.A) interestingLog Cairo.PNG (webpath ++ "/" ++ "graphixthree1a.png")
          P.plotSelectedLogData (P.GraphixThree1 GT.B) interestingLog Cairo.PNG (webpath ++ "/" ++ "graphixthree1b.png")
          P.plotSelectedLogData (P.GraphixThree1 GT.C) interestingLog Cairo.PNG (webpath ++ "/" ++ "graphixthree1c.png")
          now5 <- getZonedTime
          putStrLn $ show now5 ++ " : plotted GraphixThree1 data"

        when gt2Sw $ do
          P.plotSelectedLogData (P.GraphixThree2 GT.A) interestingLog Cairo.PNG (webpath ++ "/" ++ "graphixthree2a.png")
          P.plotSelectedLogData (P.GraphixThree2 GT.B) interestingLog Cairo.PNG (webpath ++ "/" ++ "graphixthree2b.png")
          P.plotSelectedLogData (P.GraphixThree2 GT.C) interestingLog Cairo.PNG (webpath ++ "/" ++ "graphixthree2c.png")
          now6 <- getZonedTime
          putStrLn $ show now6 ++ " : plotted GraphixThree2 data"
      else do
        now7 <- getZonedTime
        putStrLn $ show now7 ++ " : can not plot -> would need " ++ show linesToTake ++ " lines, but i have only " ++ show (length interestingLogLines) ++ " lines"


    now8 <- getZonedTime
    putStrLn $ show now8 ++ " : finished update cycle"

    threadDelay currentUpdateInterval
