-- | Module for talking to the Vacom Coldion CU-100. A serial RS232 port
-- | is used for communication. 24 Bytes are sent and received through
-- | this interface. For description of the protocoll see:
-- | https://www.vacom.de/en/downloads/category/775-total-pressure-measurement?download=3038:vacom-protocol-for-beginners
-- | https://www.vacom.de/en/downloads/category/775-total-pressure-measurement?download=2901:coldion-cu-100-manual
-- |
-- | No IO is done within this module, everything here is pure and very
-- | specifically tuned to be used with Vacom gauges

{-# LANGUAGE TemplateHaskell #-}

module Hardware.Vacom.Coldion
( CICommand(..)
, CIString(..)
, dataBytes2List
, list2DataBytes
, ciString2ByteString
, createCommandCIString
, parseAnswer
, parsePressure
, parseName
) where
import           Lens.Micro.TH
import Lens.Micro
import qualified Data.ByteString                  as B
import           Data.Attoparsec.ByteString.Char8
import           Data.Attoparsec.ByteString.Lazy
import           Data.Maybe
import           Data.Word
import           Internal.BinaryMessages

-- | commands known to the Coldion AND this programm
data CICommand =
    AskPressure Word8
  | GetDevName
  deriving (Show, Eq)

isAskPressure :: CICommand -> Bool
isAskPressure (AskPressure _ ) = True
isAskPressure _                = False

fromAskPressure :: CICommand -> Maybe Word8
fromAskPressure (AskPressure a) = Just a
fromAskPressure _               = Nothing


-- | the protocol transmits exactly 16 Bytes of data
type DataBytes =
  ( Word8, Word8, Word8, Word8
  , Word8, Word8, Word8, Word8
  , Word8, Word8, Word8, Word8
  , Word8, Word8, Word8, Word8 )

-- | convert between the 16 element tuple "DataBytes" and a list of Word8
dataBytes2List :: DataBytes -> [Word8]
dataBytes2List d =
  [ b1 , b2 , b3 , b4
  , b5 , b6 , b7 , b8
  , b9 , b10, b11, b12
  , b13, b14, b15, b16 ]
  where
    b1  = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) d
    b2  = (\(_, a, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) d
    b3  = (\(_, _, a, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) d
    b4  = (\(_, _, _, a, _, _, _, _, _, _, _, _, _, _, _, _) -> a) d
    b5  = (\(_, _, _, _, a, _, _, _, _, _, _, _, _, _, _, _) -> a) d
    b6  = (\(_, _, _, _, _, a, _, _, _, _, _, _, _, _, _, _) -> a) d
    b7  = (\(_, _, _, _, _, _, a, _, _, _, _, _, _, _, _, _) -> a) d
    b8  = (\(_, _, _, _, _, _, _, a, _, _, _, _, _, _, _, _) -> a) d
    b9  = (\(_, _, _, _, _, _, _, _, a, _, _, _, _, _, _, _) -> a) d
    b10 = (\(_, _, _, _, _, _, _, _, _, a, _, _, _, _, _, _) -> a) d
    b11 = (\(_, _, _, _, _, _, _, _, _, _, a, _, _, _, _, _) -> a) d
    b12 = (\(_, _, _, _, _, _, _, _, _, _, _, a, _, _, _, _) -> a) d
    b13 = (\(_, _, _, _, _, _, _, _, _, _, _, _, a, _, _, _) -> a) d
    b14 = (\(_, _, _, _, _, _, _, _, _, _, _, _, _, a, _, _) -> a) d
    b15 = (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, a, _) -> a) d
    b16 = (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, a) -> a) d


list2DataBytes :: [Word8] -> DataBytes
list2DataBytes a =
  ( a !! 0 , a !! 1 , a !! 2 , a !! 3
  , a !! 4 , a !! 5 , a !! 6 , a !! 7
  , a !! 8 , a !! 9 , a !! 10, a !! 11
  , a !! 12, a !! 13, a !! 14, a !! 15 )


-- | structure of communication strings from and to the device
data CIString = CIString
  { _ciStart          :: Word8          -- the start byte (A5)
  , _ciHeader         :: Word8          -- the header byte
  , _ciAdressReceiver :: Word8          -- adress of the receiver (00 for RS232)
  , _ciAdressSender   :: Word8          -- adress of the sender (00 for RS232)
  , _ciCommand        :: (Word8, Word8) -- command and subcommand
  , _ciData           :: DataBytes      -- the data bytes
  , _ciChecksum       :: (Word8, Word8) -- the checksum, two bytes
  }
makeLenses ''CIString

-- how to print a CIString (the hexadecimal representation of each Byte)
instance Show CIString where
  show a = concat . map ((++ " ") . word8String). ciString2Word8s $ a

-- | convert a CIString to a list of Word8 (24 Byte Sequence)
ciString2Word8s :: CIString -> [Word8]
ciString2Word8s input =
  [ b0 , b1 , b2 , b3
  , b4 , b5 , b6 , b7
  , b8 , b9 , b10, b11
  , b12, b13, b14, b15
  , b16, b17, b18, b19
  , b20, b21, b22, b23 ]
  where
    b0  = _ciStart input
    b1  = _ciHeader input
    b2  = _ciAdressReceiver input
    b3  = _ciAdressSender input
    b4  = fst . _ciCommand $ input
    b5  = snd . _ciCommand $ input
    b6  = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . _ciData $ input
    b7  = (\(_, a, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . _ciData $ input
    b8  = (\(_, _, a, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . _ciData $ input
    b9  = (\(_, _, _, a, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . _ciData $ input
    b10 = (\(_, _, _, _, a, _, _, _, _, _, _, _, _, _, _, _) -> a) . _ciData $ input
    b11 = (\(_, _, _, _, _, a, _, _, _, _, _, _, _, _, _, _) -> a) . _ciData $ input
    b12 = (\(_, _, _, _, _, _, a, _, _, _, _, _, _, _, _, _) -> a) . _ciData $ input
    b13 = (\(_, _, _, _, _, _, _, a, _, _, _, _, _, _, _, _) -> a) . _ciData $ input
    b14 = (\(_, _, _, _, _, _, _, _, a, _, _, _, _, _, _, _) -> a) . _ciData $ input
    b15 = (\(_, _, _, _, _, _, _, _, _, a, _, _, _, _, _, _) -> a) . _ciData $ input
    b16 = (\(_, _, _, _, _, _, _, _, _, _, a, _, _, _, _, _) -> a) . _ciData $ input
    b17 = (\(_, _, _, _, _, _, _, _, _, _, _, a, _, _, _, _) -> a) . _ciData $ input
    b18 = (\(_, _, _, _, _, _, _, _, _, _, _, _, a, _, _, _) -> a) . _ciData $ input
    b19 = (\(_, _, _, _, _, _, _, _, _, _, _, _, _, a, _, _) -> a) . _ciData $ input
    b20 = (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, a, _) -> a) . _ciData $ input
    b21 = (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, a) -> a) . _ciData $ input
    b22 = fst . _ciChecksum $ input
    b23 = snd . _ciChecksum $ input

-- | convert a CIString to a non-lazy ByteString (8 bit sequence)
ciString2ByteString :: CIString -> B.ByteString
ciString2ByteString = B.pack . ciString2Word8s

-- | the default CIString, that will be updated
defCIString :: CIString
defCIString =
  CIString
    { _ciStart          = 0xA5
    , _ciHeader         = 0x50
    , _ciAdressReceiver = 0x00
    , _ciAdressSender   = 0x00
    , _ciCommand        = (0x00, 0x00)
    , _ciData           = ( 0x00, 0x00, 0x00, 0x00
                           , 0x00, 0x00, 0x00, 0x00
                           , 0x00, 0x00, 0x00, 0x00
                           , 0x00, 0x00, 0x00, 0x00 )
    , _ciChecksum       = (0x00, 0x00)
    }

-- | starting from the default CIString update the CIString based on the CICommand
-- | that is supplied.
-- | updates of the CIString happen using Control.Lens operators
createCommandCIString :: CICommand -> Maybe CIString
createCommandCIString a
  | isAskPressure a && fromJust pressureChannel == 0x01 =
    Just
    $ defCIString
    & ciCommand .~ (0x20, 0x10)
    & ciData .~ changeDB1 (defCIString ^. ciData) (fromJust pressureChannel)
    & ciChecksum .~ (0xFF, 0x8C)
  | isAskPressure a && fromJust pressureChannel == 0x02 =
    Just
    $ defCIString
    & ciCommand .~ (0x20, 0x10)
    & ciData .~ changeDB1 (defCIString ^. ciData) (fromJust pressureChannel)
    & ciChecksum .~ (0xBF, 0x7D)
  | a == GetDevName =
    Just
    $ defCIString
    & ciCommand .~ (0x01, 0x00)
    & ciChecksum .~ (0x69, 0xEF)
  | otherwise = Nothing
  where
    pressureChannel = fromAskPressure a
    changeDB1 :: DataBytes -> Word8 -> DataBytes
    changeDB1 d n =
      ( n  , b2 , b3 , b4
      , b5 , b6 , b7 , b8
      , b9 , b10, b11, b12
      , b13, b14, b15, b16 )
      where
        b2  = (\(_, x, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> x) d
        b3  = (\(_, _, x, _, _, _, _, _, _, _, _, _, _, _, _, _) -> x) d
        b4  = (\(_, _, _, x, _, _, _, _, _, _, _, _, _, _, _, _) -> x) d
        b5  = (\(_, _, _, _, x, _, _, _, _, _, _, _, _, _, _, _) -> x) d
        b6  = (\(_, _, _, _, _, x, _, _, _, _, _, _, _, _, _, _) -> x) d
        b7  = (\(_, _, _, _, _, _, x, _, _, _, _, _, _, _, _, _) -> x) d
        b8  = (\(_, _, _, _, _, _, _, x, _, _, _, _, _, _, _, _) -> x) d
        b9  = (\(_, _, _, _, _, _, _, _, x, _, _, _, _, _, _, _) -> x) d
        b10 = (\(_, _, _, _, _, _, _, _, _, x, _, _, _, _, _, _) -> x) d
        b11 = (\(_, _, _, _, _, _, _, _, _, _, x, _, _, _, _, _) -> x) d
        b12 = (\(_, _, _, _, _, _, _, _, _, _, _, x, _, _, _, _) -> x) d
        b13 = (\(_, _, _, _, _, _, _, _, _, _, _, _, x, _, _, _) -> x) d
        b14 = (\(_, _, _, _, _, _, _, _, _, _, _, _, _, x, _, _) -> x) d
        b15 = (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, x, _) -> x) d
        b16 = (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, x) -> x) d


-- | pxrse the answer for a pressure request
parseAnswer :: Parser CIString
parseAnswer = do
  -- parse 24 bytes of data from the answer ByteString
  answer_ciStart <- word8 0xA5
  answer_ciHeader <- word8 0x70
  answer_ciAdressReceiver <- word8 0x00
  answer_ciAdressSender <- word8 0x00
  answer_ciCommand <- anyWord8
  answer_ciSubcommand <- anyWord8
  answer_ciData_bs <- Data.Attoparsec.ByteString.Lazy.take 16
  answer_ciChecksum_bs <- Data.Attoparsec.ByteString.Lazy.take 2

  -- | transform ci_data from ByteString to DataBytes
  -- | and the checksum to a tuple
  let answer_ciData = list2DataBytes . B.unpack $ answer_ciData_bs
      answer_ciChecksum =
        ( B.unpack answer_ciChecksum_bs !! 0
        , B.unpack answer_ciChecksum_bs !! 1 )

  -- | return the answer as a CIString
  return CIString
    { _ciStart = answer_ciStart
    , _ciHeader = answer_ciHeader
    , _ciCommand = (answer_ciCommand, answer_ciSubcommand)
    , _ciAdressReceiver = answer_ciAdressReceiver
    , _ciAdressSender = answer_ciAdressSender
    , _ciData = answer_ciData
    , _ciChecksum = answer_ciChecksum
    }

-- | get the pressure from a ByteString only containing DataBytes
parsePressure :: Parser Double
parsePressure = do
    pressure <- double

    return pressure

parseName :: Parser String
parseName = do
  name <- Data.Attoparsec.ByteString.Lazy.takeTill (== 0x00)

  return $ show name
