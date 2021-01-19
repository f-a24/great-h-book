import System.IO
import Data.Char
-- import Control.Exception

-- openFile：
-- ・ファイルパスとIOModeを受け取る
-- ・ファイルを開いて関連付けられたハンドルを返すI/Oアクションを返す
-- hGetContents：
-- ・コンテンツをどのファイルから読み出すべきか知っているHandleを受け取る
-- ・ファイルに含まれる内容を結果として返すIO Stringを返す
-- ・getContentsと挙動がほぼ同じで、一度にメモリに読み込むことはしない
main = do
    handle <- openFile "chapter09/baabaa.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

-- withFile：
-- ・ファイルパスとIOModeのほかに、ハンドルを受け取ってI/Oアクションを返す関数を受け取る
-- ・ファイルを開いてから何かして閉じるI/Oアクションを返す
-- ・処理途中に何かおかしなことが起きてもファイルのハンドルを確実に閉じる
main = do
    withFile "chapter09/baabaa.txt" ReadMode $ \handle -> do
        contents <- hGetContents handle
        putStr contents

{- bracketを使用したwithFile関数
-- bracket：
-- ・Control.Exceptionモジュールに用意されている
-- ・例外処理
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile name mode f = bracket (openFile name mode)
    (\handle -> hClose handle)
    (\handle -> f handle)
-}

-- readFileを使用すれば手軽に実装可能
-- ハンドルは自動で閉じる
main = do
    contents <- readFile "chapter09/baabaa.txt"
    putStr contents

-- writeFileはファイルに文字列を書き込む
-- 指定されたファイルがすでに存在している場合、ファイルは上書きされる
-- 似たような関数でappendFileはあるが、こちらは上書きではなくファイルの末尾に追記する
main = do
    contents <- readFile "chapter09/baabaa.txt"
    writeFile "chapter09/baabaacaps.txt" (map toUpper contents)
