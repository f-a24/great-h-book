-- 行を読み込んで大文字化して表示を繰り返す

-- import Control.Monad
import Data.Char

-- main = forever $ do
--     l <- getLine
--     putStrLn $ map toUpper l

-- getContentsは遅延I/O
main = do
    contents <- getContents
    putStrLn $ map toUpper contents
