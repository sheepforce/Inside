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
--import           Data.Maybe
import           Data.Word
import           Internal.BinaryMessages

-- commands known to the Coldion AND this programm
data CICommand =
  AskPressure
  deriving Show

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


--showCIString a = "Im a CIString" :: CIString -> String
{-
https://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwiV39bZ0YnYAhXJa1AKHQm6DFIQFggoMAA&url=https%3A%2F%2Fwww.vacom.de%2Fen%2Fdownloads%2Fcategory%2F775-total-pressure-measurement%3Fdownload%3D2901%3Acoldion-cu-100-manual&usg=AOvVaw2rDyABYrgVWgiiAOU-JRJS
a command to the cold ion consists of 24 Bytes. See the Coldion manual for more
informations. The non changing body (head, check sum, ...) is built and one of
the CICommand can be given. The resulting Bytestring with 24 Bytes is built.
-}
