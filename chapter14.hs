import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Ratio
import Data.Monoid
import System.Random

-- 盗賊団の人数を引数に取り、それが大きな盗賊団であるかどうか判定する関数
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

-- ログの付いた値a -> (b, String)型の関数の2つを取り、その値の関数のほうに食わせる関数
-- applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
-- applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- ログをStringから任意のモノイドを受け付けるように変更
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

-- 食事の注文を取って、飲み物も出しくれる関数
type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ =("beer", Sum 30)
{- 実行例
ghci> ("beans", Sum 10) `applyLog` addDrink
("milk",Sum {getSum = 35})
ghci> ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink
("beer",Sum {getSum = 65})
-}

{- Control.Monad.WriterのWriter w a型の定義
newtype Writer w a = Writer { runWriter :: (a, w) }
-- Writer w a型のMonadインスタンス
instance (Monad w) => Monad (Writer a) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = fx in Writer (y, v `mappend` v`)
-}

-- Writerをdo記法で使う
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)
{- 実行結果
ghci> runWriter multWithLog
(15,["Got number: 3","Got number: 5","Gonna multiply these two"])
-}

-- ログ付きのユークリッドの互除法関数
-- ユークリッドの互除法：2つの数を取ってその最大公約数を求めるアルゴリズム
-- gcd関数は既にある
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)
{- 実行結果
ghci> fst $ runWriter (gcd' 8 3)
1      
ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
8 mod 3 = 2
3 mod 2 = 1
2 mod 1 = 0
Finished with 1
-}

-- ログの出力を逆順
-- ++を右結合ではなく左結合で使ってしまうので非効率的
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result
{- 実行結果
ghci> mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)
Finished with 1
2 mod 1 = 0
3 mod 2 = 1
8 mod 3 = 2
-}

-- 差分リストは効率の良いリスト結合をサポートする
-- 差分リストのnewtypeラッパー
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

-- 差分リストのMonoidインスタンス
-- instance Monoid (DiffList a) where 
--     mempty = DiffList (\xs -> [] ++ xs)
--     (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f(g xs))

-- GHCの8.4、baseパッケージの4.11以降では、それまで別パッケージだったData.Monoid、Data.Semigroupがbaseパッケージに移行されて、SemigroupがMonoidのスーパークラスになりました。
-- その都合で、下記の書き方でSemigroup側で中値演算子<>を定義しないとエラーが出るようになっています。
-- https://qiita.com/kurunin52/items/c555f30b4ea7362d3cf1
instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))
instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    -- mappend = (<>) ...Semigroupの演算子(<>)と規定で同じ定義になるため、省略可能

-- ログの出力を逆順（差分リストを使って効率を上げたパターン）
gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcdReverse' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result
{- 実行結果
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse' 8 3 
Finished with 1
2 mod 1 = 0    
3 mod 2 = 1    
8 mod 3 = 2
-}

-- 自然数の引数を取って、ひたすらゼロまでカウントダウンする
-- 逆向きのログを生成することでログの中では数がカウントアップする関数
-- 普通のリストパターン：遅い
finalCountDown :: Int -> Writer [String] ()
finalCountDown 0 = do
    tell ["0"]
finalCountDown x = do
    finalCountDown (x-1)
    tell [show x]
-- mapM_ putStrLn . snd . runWriter $ finalCountDown 500000

-- 差分リストパターン：速い
finalCountDown' :: Int -> Writer (DiffList String) ()
finalCountDown' 0 = do
    tell (toDiffList ["0"])
finalCountDown' x = do
    finalCountDown' (x-1)
    tell (toDiffList [show x])
-- mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown' 500000

-- 関数モナドはReaderモナドとも呼ばれる
addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)
{- do式を使わないパターン
addStuff x = let
    a = (*2) x
    b = (+10) x
    in a+b
-}

-- スタック：いくつかのデータを格納でき、pushとpopをサポートするデータ構造
type Stack = [Int]

-- スタックのてっぺんの要素を取り除く
pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

-- スタックのてっぺんに要素を積む
push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

-- スタックに3積んで、2つ値を取り出す関数
stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), newStack1) = push 3 stack
    (a, newStack2) = pop newStack1
    in pop newStack2

{- Control.Monad.StateのState s a型の定義
newtype State s a = State { runState :: s -> (a, s) }
-- State s a型のMonadインスタンス
instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in g newState
-}

-- Stateモナドを使用したpop関数
pop' :: State Stack Int
pop' = state $ \(x:xs) -> (x, xs)

-- Stateモナドを使用したpush関数
push' :: Int -> State Stack ()
push' a = state $ \xs -> ((), a:xs)

-- Stateモナドを使用したstackManip関数
-- runState stackManip [5,8,2,1]
stackManip' :: State Stack Int
stackManip' = do
    push 3
    pop
    pop

-- スタックから1つ取り出し、5だったらそのまま元に戻す、5以外なら3と8を積む関数
stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

-- stackManipの結果が100ならstackStuffを実行する関数
moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    if a == 100
        then stackStuff
        else return ()

-- Control.Monad.StateモジュールMonadState型クラスのget関数とput関数が便利
stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]

-- getとputを使ったpop関数
pop'' :: State Stack Int
pop'' = do
    (x:xs) <- get
    put xs
    return x

-- getとputを使ったpush関数
push'' :: Int -> State Stack ()
push'' x = do
    xs <- get
    put (x:xs)

-- state関数を使って状態をモナドに任せる
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

-- chapter09のthreeCoinsがきれいに書ける
-- runState threeCoins (mkStdGen 33)
threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a, b, c)

{- Control.Monad.Errorモジュールで宣言されているEitherのMonadインスタンス
instance (Error e) => Monad (Either e) where
    return x = Right x
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)
-}

{- モナディック関数：モナド値を操作したり、モナド値を返したりする関数
-- liftM関数：関数とモナド値を取って、関数でモナド値を写す（fmapそのもの）
-- join関数：入れ子のモナド値を平らにして単一のモナド値にする
-- filterM関数：述語が文脈付きBoolを返し、結果にも適切な文脈が付くfilter
-- foldM関数：foldlのモナド版
-}
-- リストを取り4より小さい要素だけを残す関数
-- TrueかFalseだけではなく、ログも残す
keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False
-- 実行
-- fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
-- ログ出力
-- mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]

-- filterMを使った冪集合
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

-- 整数のリストを加算するが、9より大きければ計算全体を失敗させる関数
binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9 = Nothing
    | otherwise = Just (acc + x)
-- foldM binSmalls 0 [2,11,3,1]

-- 安全な逆ポーランド記法電卓
-- (逆ポーランド記法電卓はchapter10参照)
foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((y * x):ys)
foldingFunction (x:y:ys) "+" = return ((y + x):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result

-- モナディック関数の合成は<=<
-- chapter13のin3を一般化
inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

-- canReachIn3を一般化
canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start

-- 分数を表すRational型（Data.Ratioモジュール）
-- 例．1%2　→　1/2、5%4　→　5/4

-- 確率を持ったリスト
newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

-- ファンクターのインスタンスにする
instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

-- 確率リストを表す関数
thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [(Prob [('a', 1%2),('b',1%2)], 1%4)
    ,(Prob [('c', 1%2),('d',1%2)], 3%4)
    ]

-- 入れ子になった確率リストを平らにする関数
-- multAll：確率リストとある確率pのタプルを取って、リストの中のすべての確率をp倍して、事象と確率の組のリストを返す関数
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs

-- Monadインスタンス
instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

-- コインの型：Heads（表）、Tails（裏）
data Coin = Heads | Tails deriving (Show, Eq)

-- 普通のコイン（表裏50%ずつ）
coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

-- 細工されたコイン（表10%、裏90%）
loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

-- 3回コインを投げる
flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])
