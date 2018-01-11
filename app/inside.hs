-- | This is the main app. It draws an terminal user interface using the Brick
-- | library. A request to all enabled devices every 2.5 seconds is hardcoded.
-- | Enabling and disabling logging, individual devices and reading a
-- | configuration file is supported. Key bindings for the interaction are shown
-- | in the ui of the program.

import           Brick
import           Brick.BChan                              (newBChan, writeBChan)
import           Control.Concurrent                       (forkIO, threadDelay)
import           Control.Monad                            (forever, void)
import           Data.Attoparsec.Text.Lazy
import           Data.Either.Unwrap
import qualified Data.Text.IO                             as T
import qualified Graphics.Vty                             as V
import           Internal.UI.Data
import           Internal.UI.EventHandling
import           Internal.UI.Parser
import           Internal.UI.Widgets
import           System.Environment


{- ########################################################################## -}
{- Define the user interface -}
{- ########################################################################## -}
-- | defining the Brick app type
app :: App Measurements Tick Name
app = App
  { appDraw         = drawUI                  -- these are the visual elements
                                              -- shown (aka layout), as well as
                                              -- the functionality they have

  , appChooseCursor = neverShowCursor         -- how to handle the cursor and
                                              -- where it is placed now

  , appHandleEvent  = handleEvent             -- a function, describing what
                                              -- happens if a specific event
                                              -- occurs (key press, time passes)

  , appStartEvent   = return                  -- directly start, dont modify
                                              -- anything before we even start

  , appAttrMap      = const theMeasurements   -- a lookup list of styles
                                              -- "const" means that it is never
                                              -- changed
  }


{- ########################################################################## -}
{- Main Part, performing IO and executing the interface -}
{- ########################################################################## -}
main :: IO ()
main = do
  -- possibly read the command line for the config file
  args <- getArgs

  -- if configFile exists and is valid, read it, otherwise use the internal
  -- defaults
  startMeasure <-
        if length args == 0
          then return initMeasurement
          else do
            confFile <- T.readFile (head args)
            let confMeasureParse = parseOnly defaultsParser confFile
            if isLeft confMeasureParse
              then do
                putStrLn "configFile is not valid, using defaults instead"
                return initMeasurement
              else return $ fromRight confMeasureParse

  -- make a second thread, that constantly feeds ticks into a channel, read by
  -- Brick, 50 ticks buffering maximal
  chan <- newBChan 50
  _ <- forkIO $ forever $ do
         writeBChan chan Tick
         threadDelay tickDistance

  -- execute the TUI with the startMeasure
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app startMeasure
