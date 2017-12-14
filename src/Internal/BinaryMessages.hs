module Internal.BinaryMessages
(
) where
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8   as C
import           Data.HexString
import           Data.Word

-- take a hexadecimal and convert to ByteString
