module Internal.Plotting
( parsePlot
-- , plotLogData
) where
import           Control.Lens
import           Data.Attoparsec.Text.Lazy
import qualified Data.Text                              as T
import           Data.Time
import           Graphics.Rendering.Chart.Backend.Cairo as Cairo
import           Graphics.Rendering.Chart.Easy

data PlotData = PlotData
  { _time           :: LocalTime
  , _ciPressure     :: Double
  , _lsTemperatures :: (Double, Double)
  , _gt1Pressures   :: (Double, Double, Double)
  , _gt2Pressures   :: (Double, Double, Double)
  } deriving (Show)

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
      time <- timeParser
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
        { _time = time
        , _ciPressure = ciP
        , _lsTemperatures = (lsAP, lsBP)
        , _gt1Pressures = (gt1AP, gt1BP, gt1CP)
        , _gt2Pressures = (gt2AP, gt2BP, gt2CP)
        }
