-- Maybe a型の値とMaybe bを返す関数を引数に取り、その関数をMaybe aに適用する関数
-- >>=と同じ（Maybe版）
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

{- Monad型クラス
-- モナド：アプリカティブファンクターの強化版
class Monad m where
    -- pureの別名
    return :: a -> m a
    -- バインド：モナド値（文脈付きの値）を取って、それに通常の値を取るがモナド値を返す関数を適用する
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y
    fail :: String -> m a
    fail msg = error msg
-- MaybeのMonadインスタンス
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing
-}

-- 綱渡り
-- バランス棒の左右にとまった鳥の数が3以内ならバランスをとれる
-- バランス棒の左右の鳥からバランスをとっているかどうか判定する
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing
{- 実行例
-- Poleを受け取ってMaybe Poleを返すので合成には>>=を使用する
ghci> return (0, 0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
Nothing
-}

-- 強制的に綱から落とす関数
-- (>> Nothing)と同じ
banana :: Pole -> Maybe Pole
banana _ = Nothing

-- do記法を使うと面倒なラムダ式を多く書かなくて済む
foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)
{- do記法なしパターン
foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))
-}

-- 綱渡りもdo記法で書ける
routine :: Maybe Pole
routine = do
    start <- return (0, 0)
    first <- landLeft 2 start
    Nothing -- 強制的に綱から落とす
    second <- landRight 2 first
    landLeft 1 second
-- do記法を使わないパターン
-- return (0, 0) >>= landLeft 2 >>= Nothing >>= landRight 2 >>= landLeft 1
{- Maybeのモナドとしての側面を利用しないパターン
routine :: Maybe Pole
routine =
    case Just (0, 0) of
        Nothing -> Nothing
        Just start -> case landLeft 2 start of
            Nothing -> Nothing
            Just first -> case landRight 2 first of
                Nothing -> Nothing
                Just second -> landLeft 1 second
-}
-- do式におけるパターンマッチの例
justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

-- パターンマッチに失敗するdo式
wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x

{- リストのMonadインスタンス
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []
-}

-- リストを連結してタプルのリストにする関数
listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a','b']
    return (n, ch)
{- 実行結果
ghci> listOfTuples
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-}
-- (>>=)パターン
-- [1,2] >== \n -> ['a','b'] >>= \ch -> return (n, ch)
-- リスト内包表記はリストモナドの糖衣構文
-- [(n, ch) | n <- [1,2], ch <- ['a','b']]

{- MonadPlus型クラス
-- モノイドの性質をあわせ持つモナドを表す方
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

-- リストのMonadPlusインスタンス
instance MonadPlus [] where
    mzero = []
    mplus = (++)

-- guard関数の定義
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

-- guardを使って解の候補をふるい落とすことができる
-- [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
-}
-- guardを使ったdo記法
sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x
-- リスト内包表記を使ったパターン
-- [x | x <- [1..50], '7', `elem` show x]

-- チェス盤上のナイトを3回動かして特定のマスまで移動させられるか
-- ナイトの現在位置
type KnightPos = (Int, Int)

-- ナイトの現在地を取って次に行ける位置を列挙する関数
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-2,r+2)
                ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')
{- リストモナドを使わないパターン
moveKnight (c, r) = filter onBoard
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-2,r+2)
    ]
    where onBoard (c, r) = c `elem` [1..8] && r `elem` [1..8]
-}

-- 3手で行ける位置をすべて返す関数
in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
{- do記法を使ったパターン
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second
-}

-- 2つの位置をとって1つ目の位置から2つめの位置にちょうど3手で到達できるか返す関数
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

{- モナド則
-- 左恒等性：return x >>= fとf xは等価
-- 右恒等性：m >>= returnはただのm
-- 結合法則：(m >>= f) >>= gとm >>= (\x -> f x >>= g)は等価
-}
