-- | https://www.leyboldproducts.com/media/pdf/0f/9e/dc/300550402_002_C1_GRAPHIX_123_EN.pdf

module Hardware.Leybold.GraphixThree
( Channel(..)
, GTCommand(..)
, createCommandGTString
, parsePressure
) where
import           Data.Attoparsec.ByteString.Char8
import           Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as C
import           Data.Maybe
import           Data.Word

-- | the Channel (Gauge) to send a command to
data Channel =
    A
  | B
  | C
  deriving (Show, Eq)

-- | commands known to the Graphix Three AND this programm
data GTCommand =
  AskPressure Channel
  deriving (Show, Eq)

isAskPressure :: GTCommand -> Bool
isAskPressure (AskPressure _) = True
isAskPressure _               = False

fromAskPressure :: GTCommand -> Maybe Channel
fromAskPressure (AskPressure a) = Just a
fromAskPressure _               = Nothing

-- | make the body of a message
createCommandGTStringUnChecked :: GTCommand -> Maybe C.ByteString
createCommandGTStringUnChecked a
  | a == AskPressure A = Just $ C.pack "\SI1;29 "
  | a == AskPressure B = Just $ C.pack "\SI2;29 "
  | a == AskPressure C = Just $ C.pack "\SI3;29 "
  | otherwise = Nothing

-- | make a complete message for the GraphixThree, including termination
-- | character and checksum
createCommandGTString :: GTCommand -> Maybe B.ByteString
createCommandGTString a =
  if isNothing messageBodyMaybe
    then Nothing
    else Just $ B.pack $ messabeBody ++ [messageCRC] ++ [messageEOF]
  where
    messageBodyMaybe = createCommandGTStringUnChecked a
    messabeBody = B.unpack . fromJust $ messageBodyMaybe
    messageCRC = crc messabeBody
    messageEOF = 0x04

-- | calculate the checksum for a message
crc :: [Word8] -> Word8
crc a =
  if pre < 32
    then pre + 32
    else pre
  where
    decA = map fromIntegral a :: [Int]
    pre = fromIntegral $ 255 - ((sum decA) `mod` 256) :: Word8

parsePressure :: Parser Double
parsePressure = do
  _ <- word8 0x06 -- \ACK
  pressureP <- double
  _ <- anyWord8 -- the CRC
  _ <- word8 0x04 -- \EOT

  return pressureP
