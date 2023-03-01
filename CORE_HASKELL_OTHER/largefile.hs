import Data.ByteString as B
import System.IO.MMap

main = do
    bs <- mmapFileByteString "myLargeFile" Nothing
    let l = B.length bs
    print l
    -- print last 1024 bytes:
    let bs2 = B.drop (l - 1024) bs
    print (B.unpack bs2)