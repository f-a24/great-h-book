import Data.Char
import Data.List

-- ファンクターの中身を、複数の関数を使って写したいなら関数合成が一番良い
main = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line
