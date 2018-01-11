{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Internal.UI.Widgets
( theMeasurements
, drawUI
, coldionWidgetPressure
, deviceWidgetColdIon
, coldIonWarningWidget
, lakeShoreWidgetTemperature
, deviceWidgetLakeShore
, lakeShoreWarningWidget
, graphixThree1WidgetPressure
, deviceWidgetGraphixThree1
, graphixThree1WarningWidget
, graphixThree2WidgetPressure
, deviceWidgetGraphixThree2
, graphixThree2WarningWidget
) where
import           Brick
import qualified Brick.Widgets.Border       as BB
import qualified Brick.Widgets.Border.Style as BBS
import qualified Brick.Widgets.Center       as BC
import           Data.Maybe
import           Internal.UI.Data
import           Lens.Micro
import           Text.Printf
import qualified Graphics.Vty                             as V

-- | styles used for the widgets
theMeasurements :: AttrMap
theMeasurements = attrMap V.defAttr
  [ ("normalAttr"  :: AttrName, V.white `on` V.black)
  , ("warnAttr"    :: AttrName, V.blue `on` V.red)
  , ("okAttr"      :: AttrName, V.blue `on` V.green)
  , ("invalidAttr" :: AttrName, V.blue `on` V.yellow)
  ]

-- | horizontal limit (therefore size) of the widgets
hWidgetBoxSize :: Int
hWidgetBoxSize = 30

-- | the user interface, composed of individual Widgets
drawUI :: Measurements -> [Widget Name]
drawUI m =
  [ BC.center $
    -- ColdIon widgets
    (
      withBorderStyle BBS.unicodeBold
      $ BB.borderWithLabel (str "ColdIon CU-100")
      $ vBox [coldionWidgetPressure m, hLimit hWidgetBoxSize BB.hBorder, deviceWidgetColdIon m, hLimit hWidgetBoxSize BB.hBorder, coldIonWarningWidget m]
    )
  <+>
    -- LakeShore widgets
    (
      withBorderStyle BBS.unicodeBold
      $ BB.borderWithLabel (str "LakeShore 335")
      $ vBox [lakeShoreWidgetTemperature m, hLimit hWidgetBoxSize BB.hBorder, deviceWidgetLakeShore m, hLimit hWidgetBoxSize BB.hBorder, lakeShoreWarningWidget m]
    )
  <+>
    -- GraphixThree1 widgtes
    (
      withBorderStyle BBS.unicodeBold
      $ BB.borderWithLabel (str "GraphixThree (1)")
      $ vBox [graphixThree1WidgetPressure m, hLimit hWidgetBoxSize BB.hBorder, deviceWidgetGraphixThree1 m, hLimit hWidgetBoxSize BB.hBorder, graphixThree1WarningWidget m]
    )
  <+>
    -- GraphixThree2 widgtes
    (
      withBorderStyle BBS.unicodeBold
      $ BB.borderWithLabel (str "GraphixThree (2)")
      $ vBox [graphixThree2WidgetPressure m, hLimit hWidgetBoxSize BB.hBorder, deviceWidgetGraphixThree2 m, hLimit hWidgetBoxSize BB.hBorder, graphixThree2WarningWidget m]
    )
  <=>
    -- the on screen logging widget, showing infos about current events
    (
      hLimit ((hWidgetBoxSize + 2) * 4)
      $ withBorderStyle BBS.unicodeBold
      $ BB.borderWithLabel (str "On Screen Infos")
      $ padRight Max
      $ padLeftRight 2
      $ padTopBottom 2
      $ vBox [ str i | i <- m ^. onScreenInfo]
    )
  <=>
    -- static widget, only showing key bindings
    (
      hLimit ((hWidgetBoxSize + 2) * 4)
      $ withBorderStyle BBS.unicodeBold
      $ BB.borderWithLabel (str "Key Bindings")
      $ BC.hCenter
      $ padTopBottom 2
      $ str
        $  "c                    : toggle ColdIon\n"
        ++ "l                    : toggle LakeShore 355\n"
        ++ "g                    : toggle GraphixThree (1)\n"
        ++ "t                    : toggle GraphixThree (2)\n"
        ++ "o                    : toggle Logging\n"
        ++ "+                    : increase plot interval\n"
        ++ "-                    : decrease plot interval\n"
        ++ "Ctrl (c / l / g / t) : plot data for device\n"
        ++ "Space                : enter plot interval manually"

    )
  ]

{- ======= -}
{- ColdIon -}
{- ======= -}
-- | widget for holding the Coldion pressure
coldionWidgetPressure :: Measurements -> Widget Name
coldionWidgetPressure m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ str $ printf "%8s : %8.2e mbar\n\n\n" shownLabel shownPressure
  where
    shownLabel = m ^. coldIon . ciLabel
    shownPressure = fromMaybe 0.0 $ m ^. (coldIon . ciPressure)

-- | showing the parameters of the devices (ports, ...)
deviceWidgetColdIon :: Measurements -> Widget Name
deviceWidgetColdIon m =
  hLimit hWidgetBoxSize
  $ padTopBottom 1
  $ str
  $  ""
  ++ "  Enabled:\n"
  ++ "    " ++ shownEnabled ++ "\n\n"
  ++ "  Port:\n"
  ++ "    " ++ shownPort ++ "\n\n"
  ++ "  Channel:\n"
  ++ "    " ++ shownChannel ++ "\n\n"
  ++ "  Device Name:\n"
  ++ "    " ++ show shownDevName ++ "\n\n"
  ++ "  Warning Threshold:\n"
  ++ "    " ++ show shownWarningThreshold

  where
    shownEnabled = show $ m ^. (coldIon . ciEnabled)
    shownPort = m ^. (coldIon . ciPort)
    shownChannel = show $ m ^. (coldIon . ciChannel)
    shownDevName = m ^. (coldIon . ciDevName)
    shownWarningThreshold = m ^. (coldIon . ciWarnThresh)

-- | warnings for the ColdIon, coloured
coldIonWarningWidget :: Measurements -> Widget Name
coldIonWarningWidget m =
  hLimit hWidgetBoxSize
  $ withAttr attr
  $ BC.hCenter
  $ padTopBottom 1
  $ case coldIonWarning of
      OK         -> str "OK"
      OverThresh -> str "WARNING"
      Invalid    -> str "COMMUNICATION"
  where
    coldIonWarnThresh = m ^. coldIon . ciWarnThresh
    coldIonMonitorVal = m ^. coldIon . ciPressure
    coldIonWarning
      | isNothing coldIonMonitorVal = Invalid
      | fromJust coldIonMonitorVal > coldIonWarnThresh = OverThresh
      | fromJust coldIonMonitorVal <= coldIonWarnThresh = OK
      | otherwise = Invalid
    attr
      | coldIonWarning == OK = "okAttr" :: AttrName
      | coldIonWarning == OverThresh = "warningAttr" :: AttrName
      | coldIonWarning == Invalid = "invalidAttr" :: AttrName
      | otherwise = "invalidAttr" :: AttrName

{- ========= -}
{- LakeShore -}
{- ========= -}
-- | showing current temperatures for the LakeShore
lakeShoreWidgetTemperature :: Measurements -> Widget Name
lakeShoreWidgetTemperature m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ vBox
  [ str $ printf "%8s : %8.2f K\n" shownLabel1 (fromMaybe 0.0 tempA)
  , str $ printf "%8s : %8.2f K\n" shownLabel2 (fromMaybe 0.0 tempB)
  , str "\n"
  ]
  where
    maybeTemps = m ^. lakeShore . lsTemperatures
    tempA = fst maybeTemps
    tempB = snd maybeTemps
    shownLabel1 = m ^. lakeShore . lsLabel . _1
    shownLabel2 = m ^. lakeShore . lsLabel . _2

-- | device parameters of the LakeShore (port, Threshold)
deviceWidgetLakeShore :: Measurements -> Widget Name
deviceWidgetLakeShore m =
  hLimit hWidgetBoxSize
  $ padTopBottom 1
  $ str
  $  ""
  ++ "  Enabled:\n"
  ++ "    " ++ shownEnabled ++ "\n\n"
  ++ "  Port:\n"
  ++ "    " ++ shownPort ++ "\n\n"
  ++ "\n\n\n"
  ++ "  Device Name:\n"
  ++ "    " ++ show shownDevName ++ "\n\n"
  ++ "  Warning Threshold:\n"
  ++ "    " ++ show shownWarningThreshold
  where
    shownEnabled = show $ m ^. (lakeShore . lsEnabled)
    shownPort = m ^. (lakeShore . lsPort)
    shownDevName = m ^. (lakeShore . lsDevName)
    shownWarningThreshold = m ^. (lakeShore . lsWarnThresh)

-- | visual hint for warnings, coloured
lakeShoreWarningWidget :: Measurements -> Widget Name
lakeShoreWarningWidget m =
  hLimit hWidgetBoxSize
  $ withAttr attr
  $ BC.hCenter
  $ padTopBottom 1
  $ case lakeShoreWarning of
      OK         -> str "OK"
      Invalid    -> str "COMMUNICATION"
      OverThresh -> str "WARNING"
  where
    lakeShoreWarnThreshA = m ^. lakeShore . lsWarnThresh . _1
    lakeShoreWarnThreshB = m ^. lakeShore . lsWarnThresh . _2
    lakeShoreMonitorVals =
      ( m ^. lakeShore . lsTemperatures . _1
      , m ^. lakeShore . lsTemperatures . _2
      )
    lakeShoreWarning
      | isNothing (fst lakeShoreMonitorVals) ||
        isNothing (snd lakeShoreMonitorVals)    = Invalid
      | fromJust (fst lakeShoreMonitorVals) > lakeShoreWarnThreshA ||
        fromJust (snd lakeShoreMonitorVals) > lakeShoreWarnThreshB    = OverThresh
      | fromJust (fst lakeShoreMonitorVals) <= lakeShoreWarnThreshA &&
        fromJust (snd lakeShoreMonitorVals) <= lakeShoreWarnThreshB    = OK
      | otherwise = Invalid
    attr
      | lakeShoreWarning == OK = "okAttr" :: AttrName
      | lakeShoreWarning == Invalid = "invalidAttr" :: AttrName
      | lakeShoreWarning == OverThresh = "warningAttr" :: AttrName
      | otherwise = "invalidAttr" :: AttrName


{- ============== -}
{- GraphixThree 1 -}
{- ============== -}
-- | showing three pressures for the Graphix Three Controller
graphixThree1WidgetPressure :: Measurements -> Widget Name
graphixThree1WidgetPressure m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ vBox
  [ str $ printf "%8s : %8.2e mbar\n" shownLabel1 (fromMaybe 0.0 pressureA)
  , str $ printf "%8s : %8.2e mbar\n" shownLabel2 (fromMaybe 0.0 pressureB)
  , str $ printf "%8s : %8.2e mbar\n" shownLabel3 (fromMaybe 0.0 pressureC)
  ]
  where
    maybePressures = m ^. (graphixThree1 . gt1Pressures)
    pressureA = maybePressures ^. _1
    pressureB = maybePressures ^. _2
    pressureC = maybePressures ^. _3
    shownLabel1 = m ^. graphixThree1 . gt1Label . _1
    shownLabel2 = m ^. graphixThree1 . gt1Label . _2
    shownLabel3 = m ^. graphixThree1 . gt1Label . _3

-- | device parameters of the GraphixThree
deviceWidgetGraphixThree1 :: Measurements -> Widget Name
deviceWidgetGraphixThree1 m =
  hLimit hWidgetBoxSize
  $ padTopBottom 1
  $ str
  $  ""
  ++ "  Enabled:\n"
  ++ "    " ++ shownEnabled ++ "\n\n"
  ++ "  Port:\n"
  ++ "    " ++ shownPort ++ "\n\n"
  ++ "\n\n\n"
  ++ "  Device Name:\n"
  ++ "    " ++ show shownDevName ++ "\n\n"
  ++ "  Warning Threshold:\n"
  ++ "    " ++ show shownWarningThreshold
  where
    shownEnabled = show $ m ^. (graphixThree1 . gt1Enabled)
    shownPort = m ^. (graphixThree1 . gt1Port)
    shownDevName = m ^. (graphixThree1 . gt1DevName)
    shownWarningThreshold = m ^. (graphixThree1 . gt1WarnThresh)

-- | visual warnings for the graphixThree
graphixThree1WarningWidget :: Measurements -> Widget Name
graphixThree1WarningWidget m =
  hLimit hWidgetBoxSize
  $ withAttr attr
  $ BC.hCenter
  $ padTopBottom 1
  $ case graphixThree1Warning of
      OK         -> str "OK"
      Invalid    -> str "COMMUNICATION"
      OverThresh -> str "WARNING"
  where
    graphixThree1WarnThreshA = m ^. graphixThree1 . gt1WarnThresh . _1
    graphixThree1WarnThreshB = m ^. graphixThree1 . gt1WarnThresh . _2
    graphixThree1WarnThreshC = m ^. graphixThree1 . gt1WarnThresh . _3
    graphixThreeMonitorVal = m ^. graphixThree1 . gt1Pressures
    graphixThree1Warning
      | isNothing (graphixThreeMonitorVal ^. _1) ||
        isNothing (graphixThreeMonitorVal ^. _2) ||
        isNothing (graphixThreeMonitorVal ^. _3)    = Invalid
      | fromJust (graphixThreeMonitorVal ^. _1) > graphixThree1WarnThreshA ||
        fromJust (graphixThreeMonitorVal ^. _2) > graphixThree1WarnThreshB ||
        fromJust (graphixThreeMonitorVal ^. _3) > graphixThree1WarnThreshC    = OverThresh
      | fromJust (graphixThreeMonitorVal ^. _1) <= graphixThree1WarnThreshA &&
        fromJust (graphixThreeMonitorVal ^. _2) <= graphixThree1WarnThreshB &&
        fromJust (graphixThreeMonitorVal ^. _3) <= graphixThree1WarnThreshC    = OK
      | otherwise = Invalid
    attr
      | graphixThree1Warning == OK = "okAttr"   :: AttrName
      | graphixThree1Warning == Invalid = "invalidAttr" :: AttrName
      | graphixThree1Warning == OverThresh = "warningAttr" :: AttrName
      | otherwise = "invalidAttr" :: AttrName

{- ============== -}
{- GraphixThree 2 -}
{- ============== -}
graphixThree2WidgetPressure :: Measurements -> Widget Name
graphixThree2WidgetPressure m =
  hLimit hWidgetBoxSize
  $ BC.hCenter
  $ padTopBottom 1
  $ vBox
  [ str $ printf "%8s : %8.2e mbar\n" shownLabel1 (fromMaybe 0.0 pressureA)
  , str $ printf "%8s : %8.2e mbar\n" shownLabel2 (fromMaybe 0.0 pressureB)
  , str $ printf "%8s : %8.2e mbar\n" shownLabel3 (fromMaybe 0.0 pressureC)
  ]
  where
    maybePressures = m ^. (graphixThree2 . gt2Pressures)
    pressureA = maybePressures ^. _1
    pressureB = maybePressures ^. _2
    pressureC = maybePressures ^. _3
    shownLabel1 = m ^. graphixThree2 . gt2Label . _1
    shownLabel2 = m ^. graphixThree2 . gt2Label . _2
    shownLabel3 = m ^. graphixThree2 . gt2Label . _3

deviceWidgetGraphixThree2 :: Measurements -> Widget Name
deviceWidgetGraphixThree2 m =
  hLimit hWidgetBoxSize
  $ padTopBottom 1
  $ str
  $  ""
  ++ "  Enabled:\n"
  ++ "    " ++ shownEnabled ++ "\n\n"
  ++ "  Port:\n"
  ++ "    " ++ shownPort ++ "\n\n"
  ++ "\n\n\n"
  ++ "  Device Name:\n"
  ++ "    " ++ show shownDevName ++ "\n\n"
  ++ "  Warning Threshold:\n"
  ++ "    " ++ show shownWarningThreshold
  where
    shownEnabled = show $ m ^. (graphixThree2 . gt2Enabled)
    shownPort = m ^. (graphixThree2 . gt2Port)
    shownDevName = m ^. (graphixThree2 . gt2DevName)
    shownWarningThreshold = m ^. (graphixThree2 . gt2WarnThresh)

graphixThree2WarningWidget :: Measurements -> Widget Name
graphixThree2WarningWidget m =
  hLimit hWidgetBoxSize
  $ withAttr attr
  $ BC.hCenter
  $ padTopBottom 1
  $ case graphixThree2Warning of
      OK         -> str "OK"
      Invalid    -> str "COMMUNICATION"
      OverThresh -> str "WARNING"
  where
    graphixThree2WarnThreshA = m ^. graphixThree2 . gt2WarnThresh . _1
    graphixThree2WarnThreshB = m ^. graphixThree2 . gt2WarnThresh . _2
    graphixThree2WarnThreshC = m ^. graphixThree2 . gt2WarnThresh . _3
    graphixThreeMonitorVal = m ^. graphixThree2 . gt2Pressures
    graphixThree2Warning
      | isNothing (graphixThreeMonitorVal ^. _1) ||
        isNothing (graphixThreeMonitorVal ^. _2) ||
        isNothing (graphixThreeMonitorVal ^. _3)    = Invalid
      | fromJust (graphixThreeMonitorVal ^. _1) > graphixThree2WarnThreshA ||
        fromJust (graphixThreeMonitorVal ^. _2) > graphixThree2WarnThreshB ||
        fromJust (graphixThreeMonitorVal ^. _3) > graphixThree2WarnThreshC    = OverThresh
      | fromJust (graphixThreeMonitorVal ^. _1) <= graphixThree2WarnThreshA &&
        fromJust (graphixThreeMonitorVal ^. _2) <= graphixThree2WarnThreshB &&
        fromJust (graphixThreeMonitorVal ^. _3) <= graphixThree2WarnThreshC    = OK
      | otherwise = Invalid
    attr
      | graphixThree2Warning == OK = "okAttr"   :: AttrName
      | graphixThree2Warning == Invalid = "invalidAttr" :: AttrName
      | graphixThree2Warning == OverThresh = "warningAttr" :: AttrName
      | otherwise = "invalidAttr" :: AttrName
