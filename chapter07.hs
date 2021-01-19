import qualified Data.Map as Map

{- Shapes.hsでPointとShapeの後ろから(..)を削除すると下記は動作しない
import chapter07.Shapes
main = do
    print $ Circle (Point 10 20) 30
-}

-- 名前、苗字、年齢、身長、電話番号、好きなアイスクリームの味
{- 特定の情報を取り出すのが面倒…
-- 順番で指定する必要あり
data Person = Person String String Int Float String String deriving (Show)

firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstName

lastName :: Person -> String
lastName (Person _ lastName _ _ _ _) = lastName

...

-}

-- レコード構文
-- フィールドを取得する関数が自動で作ってくれる
-- 順番で指定する必要はない
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)


-- データ宣言にはクラス制約付けない
data Vector a = Vector a a a deriving (Show)

-- 2つのベクトルを加算する
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

-- 2つのベクトルの内積をとる
dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = Vector i*l + j*m + k*n

-- ベクトルをスカラー倍する
vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

-- 自動導出できる型クラスはEq、Ord、Enum、Bounded、Show、Read
-- Eqは==や/=で比較可能にする
-- Showは文字列に変換可能
-- Readは文字列から変換可能（明示的な型注釈が必要かも）
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

-- Ordは順序付け可能
-- Enumは前者関数と後者関数を持つ
-- Boundedは上限と下限を持つ
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- 電話帳の連想リスト
phoneBook :: [(String, String)]
phoneBook =
    [("hoge", "111-1111")
    ,("foo", "222-2222")
    ,("bar", "333-3333")
    ]

-- 型シノニム（型同義名）で型宣言に有益な情報を載せる
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

-- 名前と電話番号を受け取って、その名前と電話番号の組が電話帳に載っているか調べる関数
-- 型シノニムを使うことで分かりやすくなる
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- 型シノニムも型引数を取るようにできる
type AssocList k v = [(k, v)]

-- 型引数を部分適用することと新しい型コンストラクタが作れる
type IntMap = Map.Map Int

-- ロッカーが割り当てられている高校を例とする
-- ロッカー使用中かどうか
data LockerState = Taken | Free deriving (Show, Eq)
-- 暗証番号
type Code = String
-- ロッカーが使用中かどうかのフラグと、ロッカーの暗証番号へのMap
type LockerMap = Map.Map Int (LockerState, Code)

-- 暗証番号を検索する関数
-- Either String Codeにすることで成功した場合は暗証番号(Code)を、失敗した場合は失敗パターン(String)を返す
-- 失敗パターン：ロッカー番号が存在しない、または既にロッカーが使われている
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- 再帰的なデータ型も可能
data List a = Empty | Cons { listhead :: a, listTail :: List a}
    deriving (Show, Read, Eq, Ord)

-- 結合性宣言では、結合順位や左結合なのか右結合なのかを指定する
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- 2つのリストを結合する関数
infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

-- 二分探索木
-- 木構造のデータ型
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

-- 要素が1つしかない木を作るための補助関数
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- 要素xを木に挿入するための関数
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

-- ある要素が木に属しているか判定する関数
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

-- 標準ライブラリにおけるEqの定義
-- 新しい型クラスを定義するのがclass
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (X /= y)
    x /= y = not (X == y)

-- 型を型クラスのインスタンスにするのがinstance
data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

-- Showのインスタンス化
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- サブクラス化も可能
-- class (Eq a) => Num a where

-- Maybeを型クラスのインスタンスにする場合
instance (Eq m) => Eq (Meybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False

-- JavaScript的な振る舞いの型の実装
-- if (0) alert("YEAH!") else alert("NO!") // アラートでNO!と表示される
class YesNo a where
    yesno :: a -> Bool

-- インスタンスの定義
instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

-- idは引数を1つ取って同じものを返すだけの標準ライブラリ関数
instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

-- 二分探索木を表す型にも
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

-- 信号機にも
instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

-- ifの真似をしてYesNo値を取る関数
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal
        then yesResult
        else noResult

-- Functor型クラス
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- mapはリスト限定で動作するfmap
instance Functor [] where
    fmap = map

-- Functorは具体型ではなく、型コンストラクタを要求しているので(Maybe m)のようには書かない
instance Functor Meybe where
    fmap (Just x) = Just (f x)
    fmap f Nothing = Nothing

-- Treeもファンクターにできる
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-- EitherのファンクターはRight値コンストラクタが来た場合には関数が適用されるが、Left値の場合に何もしない
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
