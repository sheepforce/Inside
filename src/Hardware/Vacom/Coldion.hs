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
import           Control.Lens
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
dataBytes2List a =
  [ a^._1 , a^._2 , a^._3 , a^._4
  , a^._5 , a^._6 , a^._7 , a^._8
  , a^._9 , a^._10, a^._11, a^._12
  , a^._13, a^._14, a^._15, a^._16 ]

list2DataBytes :: [Word8] -> DataBytes
list2DataBytes a =
  ( a !! 0 , a !! 1 , a !! 2 , a !! 3
  , a !! 4 , a !! 5 , a !! 6 , a !! 7
  , a !! 8 , a !! 9 , a !! 10, a !! 11
  , a !! 12, a !! 13, a !! 14, a !! 15 )


-- | structure of communication strings from and to the device
data CIString = CIString
  { _ci_start          :: Word8          -- the start byte (A5)
  , _ci_header         :: Word8          -- the header byte
  , _ci_adressReceiver :: Word8          -- adress of the receiver (00 for RS232)
  , _ci_adressSender   :: Word8          -- adress of the sender (00 for RS232)
  , _ci_command        :: (Word8, Word8) -- command and subcommand
  , _ci_data           :: DataBytes      -- the data bytes
  , _ci_checksum       :: (Word8, Word8) -- the checksum, two bytes
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
    b0  = _ci_start input
    b1  = _ci_header input
    b2  = _ci_adressReceiver input
    b3  = _ci_adressSender input
    b4  = fst . _ci_command $ input
    b5  = snd . _ci_command $ input
    b6  = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . _ci_data $ input
    b7  = (\(_, a, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . _ci_data $ input
    b8  = (\(_, _, a, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . _ci_data $ input
    b9  = (\(_, _, _, a, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . _ci_data $ input
    b10 = (\(_, _, _, _, a, _, _, _, _, _, _, _, _, _, _, _) -> a) . _ci_data $ input
    b11 = (\(_, _, _, _, _, a, _, _, _, _, _, _, _, _, _, _) -> a) . _ci_data $ input
    b12 = (\(_, _, _, _, _, _, a, _, _, _, _, _, _, _, _, _) -> a) . _ci_data $ input
    b13 = (\(_, _, _, _, _, _, _, a, _, _, _, _, _, _, _, _) -> a) . _ci_data $ input
    b14 = (\(_, _, _, _, _, _, _, _, a, _, _, _, _, _, _, _) -> a) . _ci_data $ input
    b15 = (\(_, _, _, _, _, _, _, _, _, a, _, _, _, _, _, _) -> a) . _ci_data $ input
    b16 = (\(_, _, _, _, _, _, _, _, _, _, a, _, _, _, _, _) -> a) . _ci_data $ input
    b17 = (\(_, _, _, _, _, _, _, _, _, _, _, a, _, _, _, _) -> a) . _ci_data $ input
    b18 = (\(_, _, _, _, _, _, _, _, _, _, _, _, a, _, _, _) -> a) . _ci_data $ input
    b19 = (\(_, _, _, _, _, _, _, _, _, _, _, _, _, a, _, _) -> a) . _ci_data $ input
    b20 = (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, a, _) -> a) . _ci_data $ input
    b21 = (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, a) -> a) . _ci_data $ input
    b22 = fst . _ci_checksum $ input
    b23 = snd . _ci_checksum $ input

-- | convert a CIString to a non-lazy ByteString (8 bit sequence)
ciString2ByteString :: CIString -> B.ByteString
ciString2ByteString = B.pack . ciString2Word8s

-- | the default CIString, that will be updated
defCIString :: CIString
defCIString =
  CIString
    { _ci_start          = 0xA5
    , _ci_header         = 0x50
    , _ci_adressReceiver = 0x00
    , _ci_adressSender   = 0x00
    , _ci_command        = (0x00, 0x00)
    , _ci_data           = ( 0x00, 0x00, 0x00, 0x00
                           , 0x00, 0x00, 0x00, 0x00
                           , 0x00, 0x00, 0x00, 0x00
                           , 0x00, 0x00, 0x00, 0x00 )
    , _ci_checksum       = (0x00, 0x00)
    }

-- | starting from the default CIString update the CIString based on the CICommand
-- | that is supplied.
-- | updates of the CIString happen using Control.Lens operators
createCommandCIString :: CICommand -> Maybe CIString
createCommandCIString a
  | isAskPressure a && fromJust pressureChannel == 0x01 =
    Just
    $ defCIString
    & ci_command .~ (0x20, 0x10)
    & (ci_data . _1) .~ fromJust pressureChannel
    & ci_checksum .~ (0xFF, 0x8C)
  | isAskPressure a && fromJust pressureChannel == 0x02 =
    Just
    $ defCIString
    & ci_command .~ (0x20, 0x10)
    & (ci_data . _1) .~ fromJust pressureChannel
    & ci_checksum .~ (0xBF, 0x7D)
  | a == GetDevName =
    Just
    $ defCIString
    & ci_command .~ (0x01, 0x00)
    & ci_checksum .~ (0x69, 0xEF)
  | otherwise = Nothing
  where
    pressureChannel = fromAskPressure a

-- | parse the answer for a pressure request
parseAnswer :: Parser CIString
parseAnswer = do
  -- parse 24 bytes of data from the answer ByteString
  answer_ci_start <- word8 0xA5
  answer_ci_header <- word8 0x70
  answer_ci_adressReceiver <- word8 0x00
  answer_ci_adressSender <- word8 0x00
  answer_ci_command <- anyWord8
  answer_ci_subcommand <- anyWord8
  answer_ci_data_bs <- Data.Attoparsec.ByteString.Lazy.take 16
  answer_ci_checksum_bs <- Data.Attoparsec.ByteString.Lazy.take 2

  -- transform ci_data from ByteString to DataBytes
  -- and the checksum to a tuple
  let ci_data = list2DataBytes . B.unpack $ answer_ci_data_bs
      ci_checksum =
        ( B.unpack answer_ci_checksum_bs !! 0
        , B.unpack answer_ci_checksum_bs !! 1 )

  -- return the answer as a CIString
  return CIString
    { _ci_start = answer_ci_start
    , _ci_header = answer_ci_header
    , _ci_command = (answer_ci_command, answer_ci_subcommand)
    , _ci_adressReceiver = answer_ci_adressReceiver
    , _ci_adressSender = answer_ci_adressSender
    , _ci_data = ci_data
    , _ci_checksum = ci_checksum
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
