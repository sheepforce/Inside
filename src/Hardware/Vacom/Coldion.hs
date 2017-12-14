module Hardware.Vacom.Coldion
( CICommand
) where
import Internal.BinaryMessages

data CICommand =
  GetPressure
  deriving Show

--makeCommand :: CICommand -> Maybe [Word8] -> B.ByteString
--makeCommand command dats =
