-- | plotting routines for visualizing data stored in a log file as written by
-- | the inside app.

{-# LANGUAGE TemplateHaskell #-}

module Internal.Plotting
( PlotDevice(..)
, PlotLine(..)
, PlotData(..)
, ciTag
, lsTags
, gt1Tags
, gt2Tags
, plotDats
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

-- | single line of data from the log file
data PlotLine = PlotLine
  { _time           :: LocalTime
  , _ciPressure     :: Double
  , _lsTemperatures :: (Double, Double)
  , _gt1Pressures   :: (Double, Double, Double)
  , _gt2Pressures   :: (Double, Double, Double)
  } deriving (Show)
makeLenses ''PlotLine

-- | multiple lines of plot data and device names. used by the plot function
data PlotData = PlotData
  { _ciTag    :: String
  , _lsTags   :: (String, String)
  , _gt1Tags  :: (String, String, String)
  , _gt2Tags  :: (String, String, String)
  , _plotDats :: [PlotLine]
  }
makeLenses ''PlotData

data PlotDevice =
    ColdIon
  | LakeShore LS.Channel
  | GraphixThree1 GT.Channel
  | GraphixThree2 GT.Channel
  deriving (Show, Eq)

-- | parse the log file as written by inside
parsePlot :: Parser PlotData
parsePlot = do
  -- the header with the device names
  tags <- nameParser
  skipSpace
  -- many lines of data
  plotData <- many1 dataLineParser

  return PlotData
    { _ciTag = tags !! 0
    , _lsTags = (tags !! 1, tags !! 2)
    , _gt1Tags = (tags !! 3, tags !! 4, tags !! 5)
    , _gt2Tags = (tags !! 6, tags !! 7, tags !! 8)
    , _plotDats = plotData
    }

  where
    -- parser for the header and device names
    nameParser :: Parser [String]
    nameParser = do
      -- time tag
      _ <- char '#'
      _ <- string $ T.pack "Time"
      skipSpace
      tags <- count 9 singleNameParser

      return tags

      where
        singleNameParser :: Parser String
        singleNameParser = do
          _ <- char '#'
          tag <- manyTill anyChar (char ' ')
          skipSpace

          return tag


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
    dataLineParser :: Parser PlotLine
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

      return PlotLine
        { _time = timeP
        , _ciPressure = ciP
        , _lsTemperatures = (lsAP, lsBP)
        , _gt1Pressures = (gt1AP, gt1BP, gt1CP)
        , _gt2Pressures = (gt2AP, gt2BP, gt2CP)
        }

-- | given a device and plot data (as parsed by parsePlot e.g.) it will generate
-- | and write a graph containing the requested data for the specified device
plotSelectedLogData :: PlotDevice -> PlotData -> FileFormat -> FilePath -> IO ()
plotSelectedLogData d p format file = toFile filetype file $ do
  -- attributes for title of the plot
  layout_title .= "INSIDE " ++ show d ++ " - " ++ devTag
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

    -- the tag for a specific device which is plotted as read from the log file
    devTag
      | d == ColdIon = p ^. ciTag
      | d == LakeShore LS.A = p ^. lsTags . _1
      | d == LakeShore LS.B = p ^. lsTags . _2
      | d == GraphixThree1 GT.A = p ^. gt1Tags . _1
      | d == GraphixThree1 GT.B = p ^. gt1Tags . _2
      | d == GraphixThree1 GT.C = p ^. gt1Tags . _3
      | d == GraphixThree2 GT.A = p ^. gt2Tags . _1
      | d == GraphixThree2 GT.B = p ^. gt2Tags . _2
      | d == GraphixThree2 GT.C = p ^. gt2Tags . _3
      | otherwise = "no information available"


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

    -- intermediate type only containing the data lines
    pd = p ^. plotDats

    -- sort out all data for the selected device which are precisely zero and
    -- therefore considered invalid
    plotDataXY
      | d == ColdIon = filter (\a -> (a ^. ciPressure) /= 0.0) pd
      | d == LakeShore LS.A = filter (\a -> (a ^. lsTemperatures . _1) /= 0.0) pd
      | d == LakeShore LS.B = filter (\a -> (a ^. lsTemperatures . _2) /= 0.0) pd
      | d == GraphixThree1 GT.A = filter (\a -> (a ^. gt1Pressures . _1) /= 0.0) pd
      | d == GraphixThree1 GT.B = filter (\a -> (a ^. gt1Pressures . _2) /= 0.0) pd
      | d == GraphixThree1 GT.C = filter (\a -> (a ^. gt1Pressures . _3) /= 0.0) pd
      | d == GraphixThree2 GT.A = filter (\a -> (a ^. gt2Pressures . _1) /= 0.0) pd
      | d == GraphixThree2 GT.B = filter (\a -> (a ^. gt2Pressures . _2) /= 0.0) pd
      | d == GraphixThree2 GT.C = filter (\a -> (a ^. gt2Pressures . _3) /= 0.0) pd
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

    -- the local time and date
    plotTime = map _time plotDataXY :: [LocalTime]
    plotDataX = plotTime

    -- zip filtered times with filtered device values
    plottable = zip plotDataX plotDataY
