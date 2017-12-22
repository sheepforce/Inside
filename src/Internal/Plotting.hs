{-# LANGUAGE TemplateHaskell   #-}

module Internal.Plotting
( parsePlot
, plotSelectedLogData
) where
import           Control.Lens
import           Data.Attoparsec.Text.Lazy
import qualified Data.Text                              as T
import           Data.Time
import Data.Time.Calendar.Julian
import           Graphics.Rendering.Chart.Backend.Cairo as Cairo
import           Graphics.Rendering.Chart.Easy
import qualified Hardware.LakeShore.TemperatureController as LS
import qualified Hardware.Leybold.GraphixThree as GT
import qualified Hardware.Vacom.Coldion as CI

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


parsePlot :: Parser [PlotData]
parsePlot = do
  _ <- string $ T.pack "#Time                                 #     UHV    #       A  #       B    #GT1Label1  #GT1Label2  #GT1Label3    #GT2Label1  #GT2Label2  #GT2Label3"
  skipSpace
  plotData <- many1 dataLineParser

  return plotData

  where
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

      _ <- char '.'
      _ <- manyTill anyChar (char ' ')

      _ <- string $ T.pack "CET"

      return LocalTime
        { localDay       = fromGregorian year month day
        , localTimeOfDay = TimeOfDay hour minute (read $ show second)
        }

    dataLineParser :: Parser PlotData
    dataLineParser = do
      timeP <- timeParser
      skipSpace
      ciP <- double
      skipSpace
      lsAP <- double
      skipSpace
      lsBP <- double
      skipSpace
      gt1AP <- double
      skipSpace
      gt1BP <- double
      skipSpace
      gt1CP <- double
      skipSpace
      gt2AP <- double
      skipSpace
      gt2BP <- double
      skipSpace
      gt2CP <- double
      endOfLine

      return PlotData
        { _time = timeP
        , _ciPressure = ciP
        , _lsTemperatures = (lsAP, lsBP)
        , _gt1Pressures = (gt1AP, gt1BP, gt1CP)
        , _gt2Pressures = (gt2AP, gt2BP, gt2CP)
        }

plotSelectedLogData :: PlotDevice -> [PlotData] -> IO ()
plotSelectedLogData d p = Cairo.toFile filetype plotName $ do
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
    filetype = FileOptions
      { _fo_size = (600, 400)
      , _fo_format = Cairo.SVG
      }
    plotName = "INSIDE.svg" :: FilePath
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
    timeDaytime =
      map ((fromRational . timeOfDayToDayFraction . localTimeOfDay) . (^. time)) plotDataXY :: [Double]
    timeDate =
      map (fromIntegral . snd . toJulianYearAndDay . localDay . (^. time)) plotDataXY :: [Double]
    timeFrac = zipWith (+) timeDaytime timeDate
    plotDataX = timeFrac

    plottable = zip plotDataX plotDataY
