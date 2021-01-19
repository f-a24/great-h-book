import Data.List (break)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

-- WをPに変えたいとき、パターンマッチを繰り返せばできるが分かりずらい
changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P m n) r)

-- 木と方向のリストを取って、方向指示に従って辿り着いた位置の値を更新する
data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP' :: Directions -> Tree Char -> Tree Char
changeToP' (L:ds) (Node x l r) = Node x (changeToP' ds l) r
changeToP' (R:ds) (Node x l r) = Node x l (changeToP' ds r)
changeToP' [] (Node _ l r) = Node 'P' l (changeToP' ds r)

-- 目的地にある要素を返す関数
elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x
{- Wを更新する
ghci> let newTree = changeToP [R,L] freeTree
ghci> elemAt [R,L] newTree
'P'
-}

-- パンくず
type Breadcrumbs = [Direction]

-- 左に行く
goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

-- 右に行く
goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)

-- パンくずリストだけでは、木構造を上向きに辿るためには不十分
-- 辿る可能性のあった経路も必要
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs' a = [Crumb a]

-- 辿らなかなった経路もパンくずリストに記録するようにしたgoLeft関数
goLeft' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goLeft' (Node x l r, bs) = (l, LeftCrumb x r:bs)

-- 辿らなかなった経路もパンくずリストに記録するようにしたgoRight関数
goRight' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goRight' (Node x l r, bs) = (r, RightCrumb x l:bs)

-- 経路を戻る関数
goUp :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

-- Zipper：あるデータ構造の注目点、および周辺の情報を含んでいるデータ構造
-- ズボンのジッパーを上下させる操作に似ているから
type Zipper a = (Tree a, Breadcrumbs' a)

-- ジッパーが注目している部分木のルート要素を書き換える関数
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)
-- let newFoucus = modify (\_ -> 'P') (goRight (goLeft (freeTree, [])))

-- 空の部分木を注目している場合、注目点を空でない部分木で置換すれば、気を別の木の先端に継ぎ足す操作を行う
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

-- 木のてっぺんまで移動する関数
topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

-- リストのジッパー
type ListZipper a = ([a], [a])

-- リストを前後に移動する関数
goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) -> (b:xs, bs)

-- ごく単純化したファイルシステムを木で表現する
type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

-- フォルダの例
myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "bleargh"
            , File "skull_man(scary).bmp" "bleargh"
            ]
        , File "dijon_poupon.doc" "best mustard"]
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, 100t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]

-- ファイルシステムのパンくずの型
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

-- ファイルシステムのジッパー
type FSZipper = (FSItem, [FSCrumb])

-- 階層構造を上に戻る関数
fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb nama ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

-- 現在のフォルダの中にあるファイルまたはフォルダに注目点を移す関数
-- breakは述語とリストを引数に取り、リストのペアを返す関数
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

-- 注目しているファイルもしくはフォルダの名前を変更する関数
fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

-- 現在のフォルダにアイテムを新規作成する関数
fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = (Folder folderName (item:items), bs)

-- 失敗の可能性を追加したgoLeft'関数
goLeft'' :: Zipper -> Maybe (Zipper a)
goLeft'' (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft'' (Empty, _) = Nothing

-- 失敗の可能性を追加したgoRight'関数
goRight'' :: Zipper -> Maybe (Zipper a)
goRight'' (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight'' (Empty, _) = Nothing

-- 失敗の可能性を追加したgoUp関数
goUp' :: Zipper -> Maybe (Zipper a)
goUp' (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp' (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp' (_, []) = Nothing
