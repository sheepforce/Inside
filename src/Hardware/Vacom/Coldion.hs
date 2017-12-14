module Hardware.Vacom.Coldion
( CICommand
, CIString
) where
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8   as C
import           Data.Maybe
import           Data.Word
import           Internal.BinaryMessages

data CICommand =
  AskPressure
  deriving Show

data CIString = CIString
  { ci_start          :: Word8
  , ci_header         :: Word8
  , ci_adressReceiver :: Word8
  , ci_adressSender   :: Word8
  , ci_command        :: Word8
  , ci_subcommand     :: Word8
  , ci_data           :: [Word8]
  , ci_checksum       :: (Word8, Word8)
  } deriving Eq

instance Show CIString where
  show (CIString a b c d e f g h) =
    show (B.toLazyByteString $ B.word8Hex a) ++ " " ++
    show (B.toLazyByteString $ B.word8Hex b) ++ " " ++
    show (B.toLazyByteString $ B.word8Hex c) ++ " " ++
    show (B.toLazyByteString $ B.word8Hex e) ++ " " ++
    show (B.toLazyByteString $ B.word8Hex f) ++ " " ++
    show (B.toLazyByteString $ B.word8Hex g) ++ " " ++
    show (B.toLazyByteString $ B.word8Hex a) ++ " " ++
    show (B.toLazyByteString $ B.word8Hex a) ++ " "


--showCIString a = "Im a CIString" :: CIString -> String
{-
https://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwiV39bZ0YnYAhXJa1AKHQm6DFIQFggoMAA&url=https%3A%2F%2Fwww.vacom.de%2Fen%2Fdownloads%2Fcategory%2F775-total-pressure-measurement%3Fdownload%3D2901%3Acoldion-cu-100-manual&usg=AOvVaw2rDyABYrgVWgiiAOU-JRJS
a command to the cold ion consists of 24 Bytes. See the Coldion manual for more
informations. The non changing body (head, check sum, ...) is built and one of
the CICommand can be given. The resulting Bytestring with 24 Bytes is built.
-}
