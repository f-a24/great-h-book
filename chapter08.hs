import Data.Char
import Control.Monad

-- Hello,World
-- putStrLnは文字列を引数に取り、()（空のタプル、ユニット型とも言う）を結果とするI/Oアクションを返す
main = putStrLn "hello, world"

-- do構文を使ってI/Oアクションをまとめる
-- 最後の行は束縛(foo <- putStrLnのように)できない
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine -- getLineはStringを生成するI/Oアクション
    putStrLn ("Hey " ++ name ++ ", your rock!")

-- getLineはIO Stringの型を持っているので、これは動作しない
-- nameTag = "Hello, my name is " ++ getLine

-- I/Oアクションの中でletを使う
-- <-はI/Oアクションの結果に名前を束縛
-- letは純粋な式に名前を束縛
main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- 1行ずつ読み込んでは単語を逆さまにして表示する
main = do
    line <- getLine
    if null line
        then return () -- returnは純粋な値からI/Oアクションを作り出す
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

{- このようにも書けるがletを使った方が良い
main = do
    a <- return "hoge"
    putStrLn a
-}

-- いくつかの便利なI/O関数
-- putStr：改行なしで文字列を端末に表示するI/Oアクションを返す
-- putChar：文字を端末に表示するI/Oアクションを返す
-- print：基本的にputStrLn . showと同じ

-- when
-- ・Control.Monadにある関数
-- ・BoolとI/Oアクションを受け取る
-- ・Boolの値がTrueの場合は渡されたI/Oアクションと同じものを返す
-- ・Falseだった場合は何もしないreturn ()を返す
main = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input
{- whenを使わないと…
main = do
    input <- getLine
    if (input == "SWORDFISH")
        then putStrLn input
        else return ()
-}

-- sequence
-- ・I/Oアクションのリストを受け取り、それらを順に実行するI/Oアクション（シーケンス）を返す
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
{- sequenceを使わないと…
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]
-}

-- mapM
-- ・関数とリストを受け取り、リストに対して関数をマップしてシーケンスにする
-- ・mapM_はI/Oアクションの結果が必要ないときに使用する
-- ・使用例．mapM print [1,2,3]

-- forever
-- ・Control.Monadに定義されている
-- ・I/Oアクションを受け取り、そのI/Oアクションを永遠に繰り返すI/Oアクションを返す
-- ・下の例は無限にユーザからの入力を受け取り、それを大文字化して出力し続ける
main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l


-- forM
-- ・Control.Monadに定義されている
-- ・mapMに似ているが引数の順序が逆
-- ・mapMとforMは関数とリスト、どちらの引数を長く書きたいかによって使い分けるのが良い
main = do
    colors <- forM [1,2,3,4] $ \a -> do
        putStrLn $ "Which color do you associate with number " ++ show a ++ "?"
        color <- getLine
        return color -- 省略可
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
