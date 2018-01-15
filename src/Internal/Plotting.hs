-- | plotting routines for visualizing data stored in a log file as written by
-- | the inside app.

{-# LANGUAGE TemplateHaskell #-}

module Internal.Plotting
( PlotDevice(..)
, PlotData(..)
, parsePlot
, plotSelectedLogData
) where
import           Data.Attoparsec.Text.Lazy
import qualified Data.Text                                as T
import           Data.Time
import           Data.Time.Calendar.Julian
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy            hiding ((^.), _1, _2, _3)
import qualified Hardware.LakeShore.TemperatureController as LS
import qualified Hardware.Leybold.GraphixThree            as GT
import qualified Hardware.Vacom.Coldion                   as CI
import           Lens.Micro

-- | data format as read from the log file and used by the plotter
data PlotData = PlotData
  { _time           :: LocalTime
  , _ciPressure     :: Double
  , _lsTemperatures :: (Double, Double)
  , _gt1Pressures   :: (Double, Double, Double)
  , _gt2Pressures   :: (Double, Double, Double)
  } deriving (Show)
makeLenses ''PlotData

data PlotDevice =
    ColdIon
  | LakeShore LS.Channel
  | GraphixThree1 GT.Channel
  | GraphixThree2 GT.Channel
  deriving (Show, Eq)

-- | parse the log file as written by inside
parsePlot :: Parser [PlotData]
parsePlot = do
  -- the header with the device names
  _ <- manyTill anyChar endOfLine
  skipSpace
  -- many lines of data
  plotData <- many1 dataLineParser

  return plotData

  where
    -- parser for the time at the beginning of each line
    timeParser :: Parser LocalTime
    timeParser = do
      -- date
      skipSpace
      year <- decimal
      _ <- char '-'
      month <- decimal
      _ <- char '-'
      day <- decimal

      _ <- char ' '

      -- time
      hour <- decimal
      _ <- char ':'
      minute <- decimal
      _ <- char ':'
      second <- decimal :: Parser Int

      -- decimal places for seconds
      _ <- char '.'
      _ <- manyTill anyChar (char ' ')

      -- time zone
      _ <- manyTill anyChar (char ' ')

      return LocalTime
        { localDay       = fromGregorian year month day
        , localTimeOfDay = TimeOfDay hour minute (read $ show second)
        }

    -- parse a whole line of data containing all values for all devices
    dataLineParser :: Parser PlotData
    dataLineParser = do
      timeP <- timeParser
      skipSpace
      ciP <- double       -- the coldion pressure
      skipSpace
      lsAP <- double      -- lakeshore channel A temperature
      skipSpace
      lsBP <- double      -- lakeshore channel B temperature
      skipSpace
      gt1AP <- double     -- graphixThree1 channel A pressure
      skipSpace
      gt1BP <- double     -- graphixThree1 channel B pressure
      skipSpace
      gt1CP <- double     -- graphixThree1 channel C pressure
      skipSpace
      gt2AP <- double     -- graphixThree2 channel A pressure
      skipSpace
      gt2BP <- double     -- graphixThree2 channel B pressure
      skipSpace
      gt2CP <- double     -- graphixThree2 channel C pressure
      endOfLine

      return PlotData
        { _time = timeP
        , _ciPressure = ciP
        , _lsTemperatures = (lsAP, lsBP)
        , _gt1Pressures = (gt1AP, gt1BP, gt1CP)
        , _gt2Pressures = (gt2AP, gt2BP, gt2CP)
        }

-- | given a device and plot data (as parsed by parsePlot e.g.) it will generate
-- | and write a graph containing the requested data for the specified device
plotSelectedLogData :: PlotDevice -> [PlotData] -> FileFormat -> FilePath -> IO ()
plotSelectedLogData d p format file = toFile filetype file $ do
  -- attributes for title of the plot
  layout_title .= "INSIDE " ++ show d
  layout_title_style . font_size .= 20.0

  -- layout of the x axis
  layout_x_axis . laxis_title .= "time"
  layout_x_axis . laxis_title_style . font_size .= 17.5
  layout_x_axis . laxis_style . axis_label_style . font_size .= 15.0

  -- layout of the y axis
  layout_y_axis . laxis_title .= titleY
  layout_y_axis . laxis_title_style . font_size .= 17.5
  layout_y_axis . laxis_style . axis_label_style . font_size .= 15.0
  layout_y_axis . laxis_generate .= autoScaledAxis def

  -- plot data
  plot (line "am points" [plottable])

  where
    -- plot a SVG file with 600 by 400 pixel width
    filetype = FileOptions
      { _fo_size = (600, 400)
      , _fo_format = format
      }

    -- depending of the selected device the y axis has different values on it
    titleY
      | d == ColdIon = "p / mbar"
      | d == LakeShore LS.A = "T / K"
      | d == LakeShore LS.B = "T / K"
      | d == GraphixThree1 GT.A = "p / mbar"
      | d == GraphixThree1 GT.B = "p / mbar"
      | d == GraphixThree1 GT.C = "p / mbar"
      | d == GraphixThree2 GT.A = "p / mbar"
      | d == GraphixThree2 GT.B = "p / mbar"
      | d == GraphixThree2 GT.C = "p / mbar"
      | otherwise = "i have no idea what i am plotting here"

    -- sort out all data for the selected device which are precisely zero and
    -- therefore considered invalid
    plotDataXY
      | d == ColdIon = filter (\a -> (a ^. ciPressure) /= 0.0) p
      | d == LakeShore LS.A = filter (\a -> (a ^. lsTemperatures . _1) /= 0.0) p
      | d == LakeShore LS.B = filter (\a -> (a ^. lsTemperatures . _2) /= 0.0) p
      | d == GraphixThree1 GT.A = filter (\a -> (a ^. gt1Pressures . _1) /= 0.0) p
      | d == GraphixThree1 GT.B = filter (\a -> (a ^. gt1Pressures . _2) /= 0.0) p
      | d == GraphixThree1 GT.C = filter (\a -> (a ^. gt1Pressures . _3) /= 0.0) p
      | d == GraphixThree2 GT.A = filter (\a -> (a ^. gt2Pressures . _1) /= 0.0) p
      | d == GraphixThree2 GT.B = filter (\a -> (a ^. gt2Pressures . _2) /= 0.0) p
      | d == GraphixThree2 GT.C = filter (\a -> (a ^. gt2Pressures . _3) /= 0.0) p
      | otherwise = error "you have requested to plot data of a device which is unkown"

    -- extract device- (and channel-) data from the cleaned data (y-values)
    plotDataY
      | d == ColdIon = map (^. ciPressure) plotDataXY
      | d == LakeShore LS.A = map (^. lsTemperatures . _1) plotDataXY
      | d == LakeShore LS.B = map (^. lsTemperatures . _2) plotDataXY
      | d == GraphixThree1 GT.A = map (^. gt1Pressures . _1) plotDataXY
      | d == GraphixThree1 GT.B = map (^. gt1Pressures . _2) plotDataXY
      | d == GraphixThree1 GT.C = map (^. gt1Pressures . _3) plotDataXY
      | d == GraphixThree2 GT.A = map (^. gt2Pressures . _1) plotDataXY
      | d == GraphixThree2 GT.B = map (^. gt2Pressures . _2) plotDataXY
      | d == GraphixThree2 GT.C = map (^. gt2Pressures . _3) plotDataXY
      | otherwise = error "you have requested to plot data of a device which is unkown"

    -- the time of the day as doubles (0.0=0:00, 0.5=12:00 and 1.0=24:00)
    timeDaytime =
      map ((fromRational . timeOfDayToDayFraction . localTimeOfDay) . (^. time)) plotDataXY :: [Double]

    -- the day, counted from the 01.01. of each year, converted from Int to Double
    timeDate =
      map (fromIntegral . snd . toJulianYearAndDay . localDay . (^. time)) plotDataXY :: [Double]

    -- the time as double is the x-axis
    timeFrac = zipWith (+) timeDaytime timeDate
    plotDataX = timeFrac

    -- zip filtered times with filtered device values
    plottable = zip plotDataX plotDataY
