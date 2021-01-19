import Data.Monoid
import qualified Data.Foldable as F

{- newtypeキーワード
-- 既存の型から新たな型を作る
-- 1つの型を取り、それを何かにくるんで別の型に見せかけたいという場合の為に作られたもの
-- dataキーワードより高速
-- 値コンストラクタは1種類だけしか作れず、フィールドも1つだけという制限
-- derivingキーワードも使える
-}
newtype CharList a = CharList { getCharList :: [Char] } deriving (Eq, Show)

-- newtypeを使った型クラスのインスタンス
newtype Pair b a = Pair { getPair :: (a, b) }
instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y)

-- helloMe undefinedでエラー
-- 複数の値コンストラクタがあるかもしれないので引数が評価される
data CoolBool = CoolBool { getCoolBool :: Bool }
-- helloMe undefinedでエラーは起きない
-- 値コンストラクタが1つなので_は引数が評価されない
-- newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

{- type・newtype・dataの使い分け
-- 型シグネチャを整理したいとか、型名が体を表すようにしたいだけならtype
-- 既存の型をある型クラスのインスタンスにしたいならnewtype
-- 何かまったく新しいものを作るならdata
-}

{-
-- 結合的な性質の例（*と++）とは…
　・関数を引数を2つ取る
　・2つの引数および返り値の型はすべて等しい
　・2引数関数を施して相手を変えないような特殊な値が存在する
など
-- モノイドは結合的な2引数関数と、その演算に関する単位元からなる構造
　・単位元：*における1や++における[]
-- Monoid型クラス
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat :: foldr mappend mempty
-- モノイド則：モノイドが満たすべき法則
　・mempty `mappend` x = x
　・x `mappend` mempty = x
　・(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
-- モノイドのインスタンス（リスト）
instance Monoid [a] where
    mempty = []
    mappend = (++)
-- 0を単位元にすれば+もモノイド側に成り立つ
-- モノイドにする方法が2つあるのであれば、newtypeに包んで新しい型をインスタンスにする
-- Product型
newtype Product a = Product { getProduct :: a } deriving (Eq, Ord, Read, Show, Bounded)
-- ProductのMonoidインスタンス
instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)
-- SumはProductとほぼ同じ（Product：*かSum：+の違い）
-- Num aの他にBoolもモノイドにする方法が2つある
-- ||
newtype Any = Any { getAny :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)
-- &&
newtype All = All { getAll :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid All where
    mempty = All True
    Any x `mappend` All y = All (x && y)
-- Orderingモノイド
instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT
-}

-- 2つの文字列を引数に取り、その長さを比較してOrderingを返す関数
-- 長さが等しいときは辞書順比較
-- モノイドの知識を使えばシンプルに書ける
lengthCompare :: String -> String -> Ordering
-- lengthCompare x y = let a = length x `compare` length y
--                         b = x `compare` y
--                     in if a == EQ then b else a
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

-- 母音の数の比較も追加パターン
lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
                    where vowels = length . filter (`elem` "aeiou")

{- Maybeモノイド
instance Monoid a =. Monoid (Maybe a) where
    mempty = Nothing
    Nothing = `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
-}

-- chapter07で作った木構造
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

-- TreeをFoldableのインスタンスに
instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r

testTree = Node 5
            (Node 3
                (Node 1 EmptyTree EmptyTree)
                (Node 6 EmptyTree EmptyTree)
            )
            (Node 9
                (Node 8 EmptyTree EmptyTree)
                (Node 10 EmptyTree EmptyTree)
            )
{-実行結果
ghci> F.foldl (+) 0 testTree
42
ghci> F.foldl (*) 1 testTree
64800
-}
