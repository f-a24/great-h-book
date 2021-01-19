import System.Environment
import System.Directory
import System.IO
import Control.Exception
import qualified Data.ByteString.Lazy as B -- 遅延bytestring
-- import qualified Data.ByteString as S -- 正格bytestring

-- ファイルコピーをする
main = do
    (fileName:fileName2:_) <- getArgs
    copy fileName fileName2

-- bytestringを使った独自のファイルコピー関数
copy :: String -> String -> IO ()
copy source dest = do
    contents <- B.readFile source
    bracketOnError
        (openTempFile "chapter09", "")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            B.hPutStr tempHandle contents
            hClose tempHandle
            renameFile tempName dest)
