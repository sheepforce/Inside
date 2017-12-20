-- | Module for talking to the LakeShore 335 TemperatureController. A USB
-- | interface, which emulates a serial RS232  port
{-# LANGUAGE TemplateHaskell #-}

module Hardware.LakeShore.TemperatureController
( Channel(..)
, LSCommand(..)
, createCommandLSString
, parseTemperature
) where
import           Data.Attoparsec.ByteString.Char8
import           Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as C
import           Data.Maybe

-- | the two channels that are measured when aksing for a temperature
data Channel = A | B deriving (Show, Eq)

-- | Commands known to the LakeShore AND this programm
data LSCommand =
  AskTemperature Channel
  deriving (Show, Eq)

-- | is the command a query for a temperature?
isAskTemperature :: LSCommand -> Bool
isAskTemperature (AskTemperature _) = True
isAskTemperature _ = False

-- | get the channel from a temperature query
fromAskTemperature :: LSCommand -> Maybe Channel
fromAskTemperature (AskTemperature a) = Just a
fromAskTemperature _ = Nothing

-- | create bytestring, that can directly be sent to the LakeShore
createCommandLSString :: LSCommand -> Maybe B.ByteString
createCommandLSString a
  | a == AskTemperature A = Just $ C.pack "KRDG? a\r\n"
  | a == AskTemperature B = Just $ C.pack "KRDG? b\r\n"
  | otherwise = Nothing

parseTemperature :: Parser Double
parseTemperature = do
  temp <- double
  return temp
