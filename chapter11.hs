-- ユーザが入力した文字列を逆順にするfmapを使った関数
-- main = do
--     line <- fmap reverse getLine
--     putStrLn $ "You said " ++ line ++ "backwards!"
--     putStrLn $ "Yes, you really said " ++ line ++ "backwards!"
{- fmap使わないパターン
main = do line <- getLine
    let line' = reverse line
    putStrLn $ "You said " ++ line' ++ "backwards!"
    putStrLn $ "Yes, you really said " ++ line' ++ "backwards!"
-}

{- ファンクターの中身を複数の関数を使って写すパターンは下記ファイルを参照
-- chapter11/fmapping.hs

-- 実行コマンド
-- stack runghc chapter11/fmapping
-}

-- Functorのインスタンスなのに、ファンクター側を満たしていないような病的な例
data CMaybe a = CNothing | CJust Int a deriving (Show)
instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

{- idでファンクター値を写した結果と、単にidをファンクター値に適用した結果が等しくないのでファンクター側を満たしていない
ghci> fmap id (CJust 0 "haha")
CJust 1 "haha"
ghci> id (CJust 0 "haha")
CJust 0 "haha"
-}

{- アプリカティブスタイル
ghci> [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]
ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

-- アプリカティブスタイルをリストに使うと、リスト内包表記をうまく置き換えられることが多々ある
ghci> [x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]
ghci> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
[55,80,100,110]
-}

-- アプリカティブスタイルを使うとIOもスッキリ書ける
myAction :: IO String
myAction = (++) <$> getLine <*> getLine
{-
myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return a ++ b
-}

{- 関数もアプリカティブ
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
[8.0,10.0,2.5]
-}

{- ZipList
-- import Control.Applicativeが必要
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]  
[101,102,103]
ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
[5,3,3,4]
-- (,,)は(\x y z -> (x,y,z))、(,)は(\x y -> (x,y))と同じ
ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
[('d','c','r'),('o','a','a'),('g','t','t')]
-}

{- アプリカティブ則
-- pure f <*> x = fmap f x
-- pure id <*> v = v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u

-- Control.Applicativeの関数liftA2
ghci> liftA2 (:) (Just 3) (Just [4])
Just [3,4]
-- 同じ
ghci> (:) <$> Just 3 <*> Just [4]
Just [3,4]
-}

-- アプリカティブ値のリストを取ってリストを返り値として持つ1つのアプリカティブ値を返す関数
-- ※sequenceAは常に定義済み
sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs
{- 実行例
ghci> sequenceA' [Just 3, Just 2, Just 1]
Just [3,2,1]
ghci> sequenceA' [Just 3, Nothing, Just 1]
Nothing
ghci> sequenceA' [(+3),(+2),(+1)] 3       
[6,5,4]
ghci> sequenceA' [[1,2,3],[4,5,6]] 
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
ghci> sequenceA' [[1,2,3],[4,5,6],[3,4,4],[]]
[]

-- ある数がいくつかの性質をすべて満たしているかテストしたいときなどに便利
ghci> and $ sequenceA' [(>4),(<10),odd] 7
True
-- 使わないパターン
ghci> and $ map (\f -> f 7) [(>4),(<10),odd]
True

-- I/OアクションgetLineを結合できる
ghci> sequenceA' [getLine, getLine, getLine] 
hoge
foo
piyo
["hoge","foo","piyo"]
-}
