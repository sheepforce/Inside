-- | https://www.leyboldproducts.com/media/pdf/0f/9e/dc/300550402_002_C1_GRAPHIX_123_EN.pdf

module Hardware.Leybold.GraphixThree
( Channel(..)
, GTCommand(..)
, createCommandGTString
) where
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word

-- | the Channel (Gauge) to ask
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
isAskPressure _ = False

fromAskPressure :: GTCommand -> Maybe Channel
fromAskPressure (AskPressure a) = Just a
fromAskPressure _ = Nothing

createCommandGTStringUnChecked :: GTCommand -> C.ByteString
createCommandGTStringUnChecked a
  | a == AskPressure A = C.pack "\SI1;29 "
  | a == AskPressure B = C.pack "\SI2;29 "
  | a == AskPressure B = C.pack "\SI3;29 "

createCommandGTString :: GTCommand -> B.ByteString
createCommandGTString a = B.pack $ messageBody ++ [messageCRC] ++ [messageEOF]
  where
    messageBody = B.unpack $ createCommandGTStringUnChecked a
    messageCRC = crc messageBody
    messageEOF = 0x04

crc :: [Word8] -> Word8
crc a =
  if pre < 32
    then pre + 32
    else pre
  where
    decA = map fromIntegral a :: [Int]
    pre = fromIntegral $ 255 - ((sum decA) `mod` 256) :: Word8
