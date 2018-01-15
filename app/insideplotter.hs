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
--import           System.Directory
--import           System.IO

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
  , coldIon        = True         &= help "switch to not plot graph for the ColdIon"         &= typ "BOOL"
  , lakeShore      = True         &= help "switch to not plot graphs for the LakeShore"      &= typ "BOOL"
  , graphixThree1  = True         &= help "switch to not plot graphs for the GraphixThree 1" &= typ "BOOL"
  , graphixThree2  = True         &= help "switch to not plot graphs for the GraphixThree 2" &= typ "BOOL"
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
      ciSw = coldIon arguments
      lsSw = lakeShore arguments
      gt1Sw = graphixThree1 arguments
      gt2Sw = graphixThree2 arguments
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
    putStrLn $ show now2 ++ " : reading in log files with names: " ++ insideOldLogName ++ " , " ++ insideOldLogPath
    -- read the log files
    logContent <- catch (T.readFile insideLogPath) logReadHandler
    oldLogContent <- catch (T.readFile insideOldLogPath) logReadHandler

    -- try parsing the log files and combining results
    let logP = parseOnly P.parsePlot logContent
        oldLogP = parseOnly P.parsePlot oldLogContent
        logToday
          | isRight logP = fromRight logP
          | otherwise = []
        logYesterday
          | isRight oldLogP = fromRight oldLogP
          | otherwise = []
        completeLog = logYesterday ++ logToday

        -- number of lines to read from the plot file
        insideInterval = 2.5 -- delay between updates in the main app in seconds
        linesPerMinute = (round :: Double -> Int) $ 60.0 / insideInterval
        linesToTake = currentPlotInterval * linesPerMinute

        -- the relevant part of the log file for the given time to be plotted
        interestingLog = drop (length completeLog - linesToTake) completeLog

    if (length interestingLog >= linesToTake)
      then do
        when ciSw $ do
          P.plotSelectedLogData P.ColdIon interestingLog (webpath ++ "/" ++ "coldion.svg")
          now3 <- getZonedTime
          putStrLn $ show now3 ++ " : plotted ColdIon data"

        when lsSw $ do
          P.plotSelectedLogData (P.LakeShore LS.A) interestingLog (webpath ++ "/" ++ "lakeshorea.svg")
          P.plotSelectedLogData (P.LakeShore LS.B) interestingLog (webpath ++ "/" ++ "lakeshoreb.svg")
          now4 <- getZonedTime
          putStrLn $ show now4 ++ " : plotted LakeShore data"

        when gt1Sw $ do
          P.plotSelectedLogData (P.GraphixThree1 GT.A) interestingLog (webpath ++ "/" ++ "graphixthree1a.svg")
          P.plotSelectedLogData (P.GraphixThree1 GT.B) interestingLog (webpath ++ "/" ++ "graphixthree1b.svg")
          P.plotSelectedLogData (P.GraphixThree1 GT.C) interestingLog (webpath ++ "/" ++ "graphixthree1c.svg")
          now5 <- getZonedTime
          putStrLn $ show now5 ++ " : plotted GraphixThree1 data"

        when gt2Sw $ do
          P.plotSelectedLogData (P.GraphixThree2 GT.A) interestingLog (webpath ++ "/" ++ "graphixthree2a.svg")
          P.plotSelectedLogData (P.GraphixThree2 GT.B) interestingLog (webpath ++ "/" ++ "graphixthree2b.svg")
          P.plotSelectedLogData (P.GraphixThree2 GT.C) interestingLog (webpath ++ "/" ++ "graphixthree2c.svg")
          now6 <- getZonedTime
          putStrLn $ show now6 ++ " : plotted GraphixThree2 data"
      else do
        now7 <- getZonedTime
        putStrLn $ show now7 ++ " : can not plot -> would need " ++ show linesToTake ++ " lines, but i have only " ++ show (length interestingLog) ++ " lines"


    now8 <- getZonedTime
    putStrLn $ show now8 ++ " : finished update cycle"

    threadDelay currentUpdateInterval
