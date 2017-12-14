module Hardware.Vacom.Coldion
( CICommand
, CIString
) where
--import qualified Data.ByteString               as B
--import qualified Data.ByteString.Builder       as B
--import qualified Data.ByteString.Char8         as C
--import qualified Data.ByteString.Conversion.To as B
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
    (word8String a) ++ " " ++
    word8String b ++ " " ++
    word8String c ++ " " ++
    word8String d ++ " " ++
    (word8String . fst $ e) ++ " " ++
    (word8String . snd $ e) ++ " " ++
    (word8String . (\(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, a, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, a, _, _, _, _, _, _, _, _, _, _, _, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, _, a, _, _, _, _, _, _, _, _, _, _, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, a, _, _, _, _, _, _, _, _, _, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, a, _, _, _, _, _, _, _, _, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, a, _, _, _, _, _, _, _, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, a, _, _, _, _, _, _, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, a, _, _, _, _, _, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, a, _, _, _, _, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, _, a, _, _, _, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, _, _, a, _, _, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, _, _, _, a, _, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, _, _, _, _, a, _, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, a, _) -> a) $ f) ++ " " ++
    (word8String . (\(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, a) -> a) $ f) ++ " " ++
    (word8String . fst $ g) ++ " " ++
    (word8String . snd $ g)




--showCIString a = "Im a CIString" :: CIString -> String
{-
https://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwiV39bZ0YnYAhXJa1AKHQm6DFIQFggoMAA&url=https%3A%2F%2Fwww.vacom.de%2Fen%2Fdownloads%2Fcategory%2F775-total-pressure-measurement%3Fdownload%3D2901%3Acoldion-cu-100-manual&usg=AOvVaw2rDyABYrgVWgiiAOU-JRJS
a command to the cold ion consists of 24 Bytes. See the Coldion manual for more
informations. The non changing body (head, check sum, ...) is built and one of
the CICommand can be given. The resulting Bytestring with 24 Bytes is built.
-}
