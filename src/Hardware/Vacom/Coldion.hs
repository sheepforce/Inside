{-# LANGUAGE TemplateHaskell #-}
module Hardware.Vacom.Coldion
( CICommand
, CIString
, ciString2ByteString
, createCommandCIString
) where
import           Control.Lens
import qualified Data.ByteString         as B
--import qualified Data.ByteString.Builder       as B
--import qualified Data.ByteString.Char8         as C
--import qualified Data.ByteString.Conversion.To as B
import           Data.Maybe
import           Data.Word
import           Internal.BinaryMessages

-- | commands known to the Coldion AND this programm
data CICommand =
  AskPressure Word8
  deriving (Show, Eq)

type DataBytes =
  ( Word8, Word8, Word8, Word8
  , Word8, Word8, Word8, Word8
  , Word8, Word8, Word8, Word8
  , Word8, Word8, Word8, Word8 )

isAskPressure :: CICommand -> Bool
isAskPressure (AskPressure _ ) = True
isAskPressure _                = False

fromAskPressure :: CICommand -> Maybe Word8
fromAskPressure (AskPressure a) = Just a
fromAskPressure _               = Nothing

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
    , _ci_data           = ( 0x00
                           , 0x00
                           , 0x00
                           , 0x00
                           , 0x00
                           , 0x00
                           , 0x00
                           , 0x00
                           , 0x00
                           , 0x00
                           , 0x00
                           , 0x00
                           , 0x00
                           , 0x00
                           , 0x00
                           , 0x00 )
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
    & ci_data .~ (fromJust $ setNthDataByte 0 (fromJust pressureChannel))
    & ci_checksum .~ (0xFF, 0x8C)
  | otherwise = Nothing
  where
    pressureChannel = fromAskPressure a


setNthDataByte :: Int -> Word8 -> Maybe DataBytes
setNthDataByte n d
  | n == 0 =
    Just
    ( d   , 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00 )
  | n == 1 =
    Just
    ( 0x00, d   , 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00 )
  | n == 2 =
    Just
    ( 0x00, 0x00, d   , 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00 )
  | n == 3 =
    Just
    ( 0x00, 0x00, 0x00, d
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00 )
  | n == 4 =
    Just
    ( 0x00, 0x00, 0x00, 0x00
    , d   , 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00 )
  | n == 5 =
    Just
    ( 0x00, 0x00, 0x00, 0x00
    , 0x00, d   , 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00 )
  | n == 6 =
    Just
    ( 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, d   , 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00 )
  | n == 7 =
    Just
    ( 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, d
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00 )
  | n == 8 =
    Just
    ( 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , d   , 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00 )
  | n == 9 =
    Just
    ( 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, d   , 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00 )
  | n == 10 =
    Just
    ( 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, d   , 0x00
    , 0x00, 0x00, 0x00, 0x00 )
  | n == 11 =
    Just
    ( 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, d
    , 0x00, 0x00, 0x00, 0x00 )
  | n == 12 =
    Just
    ( 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , d   , 0x00, 0x00, 0x00 )
  | n == 13 =
    Just
    ( 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, d   , 0x00, 0x00 )
  | n == 14 =
    Just
    ( 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, d   , 0x00 )
  | n == 15 =
    Just
    ( 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, d)
  | otherwise = Nothing

{-
updateNthDataByte :: Int -> Word8 -> DataBytes -> DataBytes
updateNthDataByte
  ( b0 , b1 , b2 , b3
  , b4 , b5 , b6 , b7
  , b8 , b9 , b10, b11
  , b12, b13, b14, b15 ) = ( a  , b1 , b2 , b3
                           , b4 , b5 , b6 , b7
                           , b8 , b9 , b10, b11
                           , b12, b13, b14, b15 )
-}
