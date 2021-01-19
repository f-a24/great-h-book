-- 1. ファイルとストリーム
{- 下記ファイルを参照
-- chapter09/capslocker.hs
-- chapter09/shortlinesonly.hs
-- chapter09/palindrome.hs

-- 実行コマンド
-- cat chapter09/haiku.txt | stack runghc chapter09/capslocker
-- cat chapter09/shortlines.txt | stack runghc chapter09/shortlinesonly
-- cat chapter09/words.txt | stack runghc chapter09/palindrome

※【Windows（powershell）の場合】入力のリダイレクト cat hoge.txt | stack runghc hoge.hs
-}


-- 2. ファイルの読み書き
{- 下記ファイルを参照
-- chapter09/baabaa.hs

-- 実行コマンド
-- stack runghc chapter09/baabaa
-}


-- 3.ToDoリスト
{- 下記ファイルを参照
-- chapter09/appendtodo.hs
-- chapter09/deletetodo.hs

-- 実行コマンド
-- stack runghc chapter09/appendtodo
-- stack runghc chapter09/deletetodo
-}

-- 4. コマンドライン引数
{- 下記ファイルを参照
-- chapter09/arg-test.hs

-- 実行コマンド
-- stack runghc chapter09/arg-test first second w00t "multi word arg"
-}


-- 5. ToDoリストをもっと楽しむ
{- 下記ファイルを参照
-- chapter09/todo.hs

-- 実行コマンド
-- 追加　→　stack runghc chapter09/todo add todo.txt "Find the magic sword of power"
-- 閲覧　→　stack runghc chapter09/todo view todo.txt
-- 削除　→　stack runghc chapter09/todo remove todo.txt 2
-}


-- 6. ランダム性
import System.Random -- ※stack install randomが必要

-- 3回のコイントスをシミュレートする関数
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thirdCoin)


-- randomsはジェネレータを受け取って、そのジェネレータに基づく無限長のランダムな値のリストを返す
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

-- 有限のリストと新しいジェネレータを返す関数
finiteRandoms :: (RandomGen g, Random a, Num n) => n => g => ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in (value:restOfList, finalGen)

{- 下記ファイルを参照
-- cahpter09/random_string.hs
-- cahpter09/guess_the_number.hs

-- 実行コマンド
-- stack runghc chapter09/random_string
-- stack runghc chapter09/guess_the_number
-}

-- 7. bytestring
{-
-- cahpter09/bytestringcopy.hs

-- 実行コマンド
-- stack runghc bytestringcopy bart.txt bort.txt
-}
