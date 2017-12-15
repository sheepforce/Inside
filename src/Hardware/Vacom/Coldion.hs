module Hardware.Vacom.Coldion
( CICommand
, CIString
, ciString2Word8s
, ciString2ByteString
) where
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Conversion.To as B
import           Data.Maybe
import           Data.Word
import           Internal.BinaryMessages

-- commands known to the Coldion AND this programm
data CICommand =
  AskPressure Int
  deriving (Show, Eq)

-- structure of communication strings from and to the device
data CIString = CIString
  { ci_start          :: Word8          -- the start byte (A5)
  , ci_header         :: Word8          -- the header byte
  , ci_adressReceiver :: Word8          -- adress of the receiver (00 for RS232)
  , ci_adressSender   :: Word8          -- adress of the sender (00 for RS232)
  , ci_command        :: (Word8, Word8) -- command and subcommand
  , ci_data           :: ( Word8        -- the data bytes, 16 of them
                         , Word8
                         , Word8
                         , Word8
                         , Word8
                         , Word8
                         , Word8
                         , Word8
                         , Word8
                         , Word8
                         , Word8
                         , Word8
                         , Word8
                         , Word8
                         , Word8
                         , Word8 )
  , ci_checksum       :: (Word8, Word8) -- the checksum, two bytes
  }

instance Show CIString where
  show (CIString a b c d e f g) =
    word8String a ++ " " ++
    word8String b ++ " " ++
    word8String c ++ " " ++
    word8String d ++ " " ++
    (word8String . fst $ e) ++ " " ++
    (word8String . snd $ e) ++ " " ++
    (word8String . (\(x, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, x, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, x, _, _, _, _, _, _, _, _, _, _, _, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, _, x, _, _, _, _, _, _, _, _, _, _, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, x, _, _, _, _, _, _, _, _, _, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, x, _, _, _, _, _, _, _, _, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, x, _, _, _, _, _, _, _, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, x, _, _, _, _, _, _, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, x, _, _, _, _, _, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, x, _, _, _, _, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, _, x, _, _, _, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, _, _, x, _, _, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, _, _, _, x, _, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, _, _, _, _, x, _, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, x, _) -> x) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, x) -> x) $ f) ++ " " ++
    (word8String . fst $ g) ++ " " ++
    (word8String . snd $ g)

-- convert a CIString to a list of Word8 (24 Byte Sequence)
ciString2Word8s :: CIString -> [Word8]
ciString2Word8s input =
  [ b0 , b1 , b2 , b3
  , b4 , b5 , b6 , b7
  , b8 , b9 , b10, b11
  , b12, b13, b14, b15
  , b16, b17, b18, b19
  , b20, b21, b22, b23 ]
  where
    b0  = ci_start input
    b1  = ci_header input
    b2  = ci_adressReceiver input
    b3  = ci_adressSender input
    b4  = fst . ci_command $ input
    b5  = snd . ci_command $ input
    b6  = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b7  = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b8  = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b9  = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b10 = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b11 = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b12 = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b13 = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b14 = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b15 = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b16 = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b17 = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b18 = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b19 = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b20 = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b21 = (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) . ci_data $ input
    b22 = fst . ci_checksum $ input
    b23 = snd . ci_checksum $ input

-- convert a CIString to a non-lazy ByteString (8 bit sequence)
ciString2ByteString :: CIString -> B.ByteString
ciString2ByteString = B.pack . ciString2Word8s

-- create a CIString for given Commands
createCommandCIString :: CICommand -> Maybe CIString
createCommandCIString a
  | a == AskPressure 1 =
    Just CIString { ci_start          = startB
                  , ci_header         = headerB
                  , ci_adressReceiver = adressReceiverB
                  , ci_adressSender   = adressSenderB
                  , ci_command        = (0x20, 0x10)
                  , ci_data           = ( 0x01
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
                  , ci_checksum       = (0xFF, 0x8C)
                  }
  | a == AskPressure 2 =
    Just CIString { ci_start          = startB
                  , ci_header         = headerB
                  , ci_adressReceiver = adressReceiverB
                  , ci_adressSender   = adressSenderB
                  , ci_command        = (0x20, 0x10)
                  , ci_data           = ( 0x02
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
                  , ci_checksum       = (0xBF, 0x7D)
                  }
  | otherwise          = Nothing
  where
    startB = 0xA5
    headerB = 0x50
    adressReceiverB = 0x00
    adressSenderB = 0x00
