module Internal.BinaryMessages
( word8String
, stringWord8
, word16Word8
) where
--import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Char8         as C
import           Data.ByteString.Conversion.To as B
import           Data.HexString
import           Data.Word
import Data.Bits

-- take 1 Byte (Word8) and convert to a string, giving the value of the byte
-- in hexadecimal notation
word8String :: (Word8 -> String)
word8String = C.unpack . B.toByteString' . B.word8HexFixed

-- take a hexadecimal value simply noted as string and convert it to word8
stringWord8 :: (String -> Word8)
stringWord8 = (toBinary :: HexString -> Word8) . hexString . C.pack

-- convert Word16 to 2 Word8
-- second statement works, because "fromIntegral" is clever and shifts to
-- the word8 range
word16Word8 :: Word16 -> (Word8, Word8)
word16Word8 a = (fromIntegral (shiftR a 8), fromIntegral a)
