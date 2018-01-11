module Internal.UI.Parser
( defaultsParser
, coldIonParser
, lakeShoreParser
, graphixThree1Parser
, graphixThree2Parser
) where
import qualified Data.Text as T
import Internal.UI.Data
import Lens.Micro
import Control.Applicative
import Data.Attoparsec.Text.Lazy

{- ########################################################################## -}
{- Parsers -}
{- ########################################################################## -}
-- | parsing the defaults from a file and using defaults, that are not parsed
-- | from the standard definition
defaultsParser :: Parser Measurements
defaultsParser = do
  defColdIon <- coldIonParser
  skipSpace
  defLakeShore <- lakeShoreParser
  skipSpace
  defGraphixThree1 <- graphixThree1Parser
  skipSpace
  defGraphixThree2 <- graphixThree2Parser
  skipSpace
  _ <- string $ T.pack "[Logging]"
  skipSpace
  _ <- string $ T.pack "enabled ="
  skipSpace
  enabledP <- (string $ T.pack "True") <|> (string $ T.pack "False")
  return $
    initMeasurement
    & coldIon .~ defColdIon
    & lakeShore .~ defLakeShore
    & graphixThree1 .~ defGraphixThree1
    & graphixThree2 .~ defGraphixThree2
    & writeLog .~ (enabledP == T.pack "True")

-- | parse the ColdIon block and update the defaults with the parsed result
coldIonParser :: Parser ColdIon
coldIonParser = do
  _ <- string $ T.pack "[ColdIon]"
  skipSpace
  _ <- string $ T.pack "enabled ="
  skipSpace
  enabledP <- (string $ T.pack "True") <|> (string $ T.pack "False")
  skipSpace
  _ <- string $ T.pack "port ="
  skipSpace
  portP <- manyTill anyChar endOfLine
  skipSpace
  _ <- string $ T.pack "channel ="
  skipSpace
  channelP <- decimal
  skipSpace
  _ <- string $ T.pack "warn ="
  skipSpace
  warnP <- double
  skipSpace
  _ <- string $ T.pack "label ="
  skipSpace
  label1P <- manyTill anyChar endOfLine
  return $
    initColdIon
    & ciEnabled .~ (enabledP == T.pack "True")
    & ciPort .~ portP
    & ciChannel .~ channelP
    & ciWarnThresh .~ warnP
    & ciLabel .~ label1P

-- | parsing the LakeShore part of the config file
lakeShoreParser :: Parser LakeShore
lakeShoreParser = do
  _ <- string $ T.pack "[LakeShore355]"
  skipSpace
  _ <- string $ T.pack "enabled ="
  skipSpace
  enabledP <- (string $ T.pack "True") <|> (string $ T.pack "False")
  skipSpace
  _ <- string $ T.pack "port ="
  skipSpace
  portP <- manyTill anyChar endOfLine
  skipSpace
  _ <- string $ T.pack "warn ="
  skipSpace
  warn1P <- double
  skipSpace
  warn2P <- double
  skipSpace
  _ <- string $ T.pack "label ="
  skipSpace
  label1P <- manyTill anyChar (char ' ')
  label2P <- manyTill anyChar endOfLine

  return $
    initLakeShore
    & lsEnabled .~ (enabledP == T.pack "True")
    & lsPort .~ portP
    & lsWarnThresh .~ (warn1P, warn2P)
    & lsLabel .~ (label1P, label2P)

-- | parsing the GraphixThree part of the config file
graphixThree1Parser :: Parser GraphixThree1
graphixThree1Parser = do
  _ <- string $ T.pack "[GraphixThree1]"
  skipSpace
  _ <- string $ T.pack "enabled ="
  skipSpace
  enabledP <- (string $ T.pack "True") <|> (string $ T.pack "False")
  skipSpace
  _ <- string $ T.pack "port ="
  skipSpace
  portP <- manyTill anyChar endOfLine
  skipSpace
  _ <- string $ T.pack "warn ="
  skipSpace
  warn1P <- double
  skipSpace
  warn2P <- double
  skipSpace
  warn3P <- double
  skipSpace
  _ <- string $ T.pack "label ="
  skipSpace
  label1P <- manyTill anyChar (char ' ')
  label2P <- manyTill anyChar (char ' ')
  label3P <- manyTill anyChar endOfLine

  return $
    initGraphixThree1
    & gt1Enabled .~ (enabledP == T.pack "True")
    & gt1Port .~ portP
    & gt1WarnThresh .~ (warn1P, warn2P, warn3P)
    & gt1Label .~ (label1P, label2P, label3P)

-- | parsing the GraphixThree part of the config file
graphixThree2Parser :: Parser GraphixThree2
graphixThree2Parser = do
  _ <- string $ T.pack "[GraphixThree2]"
  skipSpace
  _ <- string $ T.pack "enabled ="
  skipSpace
  enabledP <- (string $ T.pack "True") <|> (string $ T.pack "False")
  skipSpace
  _ <- string $ T.pack "port ="
  skipSpace
  portP <- manyTill anyChar endOfLine
  skipSpace
  _ <- string $ T.pack "warn ="
  skipSpace
  warn1P <- double
  skipSpace
  warn2P <- double
  skipSpace
  warn3P <- double
  skipSpace
  _ <- string $ T.pack "label ="
  skipSpace
  label1P <- manyTill anyChar (char ' ')
  label2P <- manyTill anyChar (char ' ')
  label3P <- manyTill anyChar endOfLine

  return $
    initGraphixThree2
    & gt2Enabled .~ (enabledP == T.pack "True")
    & gt2Port .~ portP
    & gt2WarnThresh .~ (warn1P, warn2P, warn3P)
    & gt2Label .~ (label1P, label2P, label3P)
